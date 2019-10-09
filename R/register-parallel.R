################################################################################

#' Register parallel
#'
#' Register parallel in functions. Do [makeCluster()], [registerDoParallel()]
#' and [stopCluster()] when the function returns.
#'
#' @param ncores Number of cores to use. If using only one, then this function
#'   uses [foreach::registerDoSEQ()].
#' @inheritDotParams makeCluster -spec
#'
#' @export
#'
#' @examples
#' test <- function(ncores) {
#'   register_parallel(ncores)
#'   foreach(i = 1:2) %dopar% i
#' }
#'
#' test(2)  # only inside the function
#' tryCatch(foreach(i = 1:2) %dopar% i, error = function(e) print(e))
#'
register_parallel <- function(ncores, ...) {

  if (identical(parent.frame(), globalenv()))
    stop2("This function must be used inside another function.")

  if (ncores == 1) {
    foreach::registerDoSEQ()
  } else {
    registerDoParallel(cl <- makeCluster(ncores, ...))
    # https://stackoverflow.com/a/20998531/6103040
    do.call("on.exit", list(substitute(stopCluster(cl)), add = TRUE),
            envir = parent.frame())
  }

  invisible(NULL)
}

################################################################################
