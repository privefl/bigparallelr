################################################################################

#' @importFrom parallel makeCluster
#' @export
parallel::makeCluster

#' @importFrom parallel stopCluster
#' @export
parallel::stopCluster

################################################################################

#' @importFrom doParallel registerDoParallel
#' @export
doParallel::registerDoParallel

################################################################################

#' @importFrom flock lock
#' @export
flock::lock

#' @importFrom flock unlock
#' @export
flock::unlock

################################################################################

#' @importFrom RhpcBLASctl blas_set_num_threads
#' @export
RhpcBLASctl::blas_set_num_threads

################################################################################
