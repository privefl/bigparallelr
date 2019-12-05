################################################################################

.onLoad <- function(libname, pkgname) {
  options(
    bigstatsr.ncores.max          = parallel::detectCores(),
    bigstatsr.check.parallel.blas = TRUE
  )
}

################################################################################

.onUnload <- function(libpath) {
  options(
    bigstatsr.ncores.max          = NULL,
    bigstatsr.check.parallel.blas = NULL,
    default.nproc.blas            = NULL
  )
}

################################################################################
