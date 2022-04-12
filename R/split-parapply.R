#' Split-parApply-Combine
#'
#' A Split-Apply-Combine strategy to parallelize the evaluation of a function.
#'
#' This function splits indices in parts, then apply a given function to each
#' part and finally combine the results.
#'
#' @param FUN The function to be applied to each subset matrix.
#' @param ind Initial vector of indices that will be splitted in `nb_split`.
#' @param ... Extra arguments to be passed to `FUN`.
#' @param .combine Function to combine the results with `do.call`.
#'   This function should accept multiple arguments (using `...`). For example,
#'   you can use `c`, `cbind` and `rbind`. This package also provides function
#'   `plus` to add multiple arguments together. The default is `NULL`, in which
#'   case the results are not combined and are returned as a list, each element
#'   being the result of a block.
#' @param nb_split Number of blocks. Default uses `1`.
#' @param .costs Vector of costs (e.g. proportional to computation time)
#'   associated with each element of `ind`. Default is `NULL` (same cost).
#'
#' @return Return a list of `nb_split` elements, each element being the result of
#'   one of the cores, computed on a block. The elements of this list are then
#'   combined with `do.call(.combine, .)` if `.combined` is not `NULL`.
#' @export
#'
#' @import foreach
#'
#' @examples
#' if (interactive()) {
#'   sys.time(str(
#'     split_parapply(function(ind) {
#'       sqrt(ind)
#'     }, ind = 1:10000, nb_split = 2)
#'   ))
#'
#'   # Using parallel
#'   library("doFuture")
#'   registerDoFuture()
#'   plan(multisession, workers = 2)
#'   system.time(str(
#'     split_parapply(function(ind) {
#'       sqrt(ind)
#'     }, ind = 1:10000, nb_split = 2)
#'   ))
#' }
#'
split_parapply <- function(FUN, ind, ..., .combine = NULL, nb_split = 1, .costs = NULL) {
  ic <- NULL # "no visible global variable"
  bigassertr::assert_args(FUN, "ind")

  if (is.null(.costs)) {
    intervals <- split_len(length(ind), nb_split = nb_split)
  } else {
    bigassertr::assert_lengths(ind, .costs)
    intervals <- split_costs(.costs, nb_split = nb_split)
  }

  res <- foreach::foreach(ic = rows_along(intervals)) %dopar% {
    ind.part <- ind[seq(intervals[ic, "lower"], intervals[ic, "upper"])]
    FUN(ind = ind.part, ...)
  }

  `if`(is.null(.combine), res, do.call(.combine, res))
}


#' Add
#'
#' Wrapper around `Reduce` to add multiple arguments. Useful
#'
#' @param ... Multiple arguments to be added together.
#'
#' @return `Reduce('+', list(...))`
#' @export
#'
#' @examples
#' plus(1:3, 4:6, 1:3)
plus <- function(...) {
  Reduce('+', list(...))
}
