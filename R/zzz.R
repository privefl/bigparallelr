################################################################################

.onLoad <- function(libname, pkgname) {
  options(
    bigstatsr.ncores.max          = parallel::detectCores(),
    bigstatsr.check.parallel.blas = TRUE,
    default.nproc.blas            = default_nproc_blas()
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
