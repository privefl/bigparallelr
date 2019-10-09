################################################################################

globalVariables("ic")

################################################################################

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
#' @param ncores Number of cores to use. Default uses `nb_cores()`.
#' @param nb_split Number of blocks. Default uses `ncores`.
#' @param opts_cluster Optional parameters for clusters passed as a named list.
#'   E.g., you can use `type = "FORK"` to use forks instead of clusters.
#'   You can also use `outfile = ""` to redirect printing to the console.#'
#'
#' @return Return a list of `ncores` elements, each element being the result of
#'   one of the cores, computed on a block. The elements of this list are then
#'   combined with `do.call(.combine, .)` if `.combined` is not `NULL`.
#' @export
#'
#' @importFrom bigassertr assert_args assert_int assert_pos
#'
#' @examples
#' str(
#'   split_parapply(function(ind) {
#'     sqrt(ind)
#'   }, ind = 1:10000, ncores = 2)
#' )
#'
split_parapply <- function(FUN, ind, ...,
                           .combine = NULL,
                           ncores = nb_cores(),
                           nb_split = ncores,
                           opts_cluster = list()) {

  assert_args(FUN, "ind")
  assert_int(ind); assert_pos(ind)
  assert_cores(ncores)

  do.call(register_parallel, args = c(list(ncores = ncores), opts_cluster))

  intervals <- split_len(length(ind), nb_split = nb_split)

  res <- foreach(ic = rows_along(intervals)) %dopar% {
    FUN(ind = ind[seq_range(intervals[ic, ])], ...)
  }

  `if`(is.null(.combine), res, do.call(.combine, res))
}

################################################################################

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

################################################################################
