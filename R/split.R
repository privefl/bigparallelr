################################################################################

#' Split in blocks
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
split_len <- function(total_len, block_len,
                      nb_split = ceiling(total_len / block_len)) {

  if (nb_split > total_len) {
    nb_split <- total_len
  } else if (nb_split == 0) {  ## block_len = Inf
    nb_split <- 1
  }
  assert_pos(nb_split); assert_int(nb_split)
  int <- total_len / nb_split

  upper <- round(1:nb_split * int)
  lower <- c(1, upper[-nb_split] + 1)
  size <- c(upper[1], diff(upper))

  cbind(lower, upper, size)
}

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
