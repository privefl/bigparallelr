################################################################################

#' Sequence generation
#'
#' - `rows_along(x)`: `seq_len(nrow(x))`
#' - `cols_along(x)`: `seq_len(ncol(x))`
#' - `seq_range(lims)`: `seq(lims[1], lims[2])`
#'
#' @param x Any object on which you can call `nrow()` and `ncol()`.
#' @param lims Vector of size 2 (or more, but only first 2 values will be used).
#'
#' @examples
#' X <- matrix(1:6, 2, 3)
#' dim(X)
#' rows_along(X)
#' cols_along(X)
#'
#' seq_range(c(3, 10))
#'
#' @rdname seq-dim
#' @keywords internal
#' @export
rows_along <- function(x) seq_len(nrow(x))

#' @rdname seq-dim
#' @export
cols_along <- function(x) seq_len(ncol(x))

################################################################################

#' @rdname seq-dim
#' @export
seq_range <- function(lims) {
  seq(lims[1], lims[2])
}

################################################################################

#' Split length in blocks
#'
#' @param total_len Length to split.
#' @param block_len Maximum length of each block.
#' @param nb_split Number of blocks. Default uses the other 2 parameters.
#'
#' @return A matrix with 3 columns `lower`, `upper` and `size`.
#' @export
#'
#' @examples
#' split_len(10, block_len = 3)
#' split_len(10, nb_split = 3)
#'
split_len <- function(total_len, block_len,
                      nb_split = ceiling(total_len / block_len)) {

  assert_one_int(total_len); assert_pos(total_len)

  if (nb_split > total_len) {
    nb_split <- total_len
  } else if (nb_split == 0) {  ## block_len = Inf
    nb_split <- 1
  }
  assert_one_int(nb_split); assert_pos(nb_split)

  int <- total_len / nb_split
  upper <- round(1:nb_split * int)
  lower <- c(1, upper[-nb_split] + 1)
  size <- c(upper[1], diff(upper))

  cbind(lower, upper, size)
}

################################################################################

get_one_block <- function(costs, nb_split) {

  n <- length(costs)

  if (nb_split == 1)
    return(list(block = cbind(1, n, n, sum(costs)), costs = NULL))

  if (nb_split >= n)
    return(list(block = cbind(seq_len(n), seq_len(n), 1, costs), costs = NULL))

  mean_cost_grp <- sum(costs) / nb_split
  cur_cost <- 0

  for (i in rev(seq_along(costs))) {
    new_cost <- cur_cost + costs[i]
    if (new_cost > mean_cost_grp) {
      if (i == n) {
        # Already pass mean_cost_grp with only one
        return(list(block = cbind(n, n, 1, new_cost),
                    costs = head(costs, i - 1L)))
      } else if ((mean_cost_grp - cur_cost) > (new_cost - mean_cost_grp)) {
        # Better to add this one
        return(list(block = cbind(i, n, n - i + 1, new_cost),
                    costs = head(costs, i - 1L)))
      } else {
        # Not better to add this one
        return(list(block = cbind(i + 1L, n, n - i, cur_cost),
                    costs = head(costs, i)))
      }
    } else {
      # Continue
      cur_cost <- new_cost
    }
  }
}

#' Split costs in blocks
#'
#' Split costs in consecutive blocks using a greedy algorithm that tries to
#' find blocks of even total cost.
#'
#' @param costs Vector of costs (e.g. proportional to computation time).
#' @param nb_split Number of blocks.
#'
#' @return A matrix with 4 columns `lower`, `upper`, `size` and `cost`.
#' @export
#'
#' @importFrom utils head
#'
#' @examples
#' split_costs(costs = 150:1, nb_split = 3)
#' split_costs(costs = rep(1, 151), nb_split = 3)
#' split_costs(costs = 150:1, nb_split = 30)
#'
split_costs <- function(costs, nb_split) {

  assert_one_int(nb_split)
  assert_pos(nb_split, strict = TRUE)
  assert_pos(costs, strict = FALSE)

  all_blocks <- list()
  while (!is.null(costs)) {
    block <- get_one_block(costs, nb_split)
    all_blocks[[length(all_blocks) + 1L]] <- block$block
    costs <- block$costs
    nb_split <- nb_split - 1L
  }

  res <- do.call("rbind", rev(all_blocks))
  colnames(res) <- c("lower", "upper", "size", "cost")

  res
}

################################################################################
