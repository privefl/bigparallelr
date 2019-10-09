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
