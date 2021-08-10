################################################################################

#' Register parallel
#'
#' Register parallel in functions. Do [makeCluster()], [registerDoParallel()]
#' and [stopCluster()] when the function returns.
#'
#' @param ncores Number of cores to use. If using only one, then this function
#'   uses [foreach::registerDoSEQ()].
#' @param ... Arguments passed on to [makeCluster()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' test <- function(ncores) {
#'   register_parallel(ncores)
#'   foreach(i = 1:2) %dopar% i
#' }
#'
#' test(2)  # only inside the function
#' foreach(i = 1:2) %dopar% i
#' }
#'
register_parallel <- function(ncores, ...) {

  assert_cores(ncores)

  if (identical(parent.frame(), globalenv()))
    stop2("This function must be used inside another function.")

  if (foreach::getDoParWorkers() > 1) {
    # User has already registered a parallel backend in their R session, so we can just use that
    # instead of setting up our own for the callee function
  } else if (ncores == 1) {
    foreach::registerDoSEQ()
  } else {
    cl <- parallel::makeCluster(ncores, ...)
    doParallel::registerDoParallel(cl)
    # https://stackoverflow.com/a/20998531/6103040
    do.call("on.exit", list(substitute(parallel::stopCluster(cl)), add = TRUE),
            envir = parent.frame())
    # https://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
    do.call("on.exit", list(substitute(foreach::registerDoSeq()), add = TRUE),
            envir = parent.frame())
  }

  invisible(NULL)
}

################################################################################
