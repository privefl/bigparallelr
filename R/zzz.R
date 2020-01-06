################################################################################

.onLoad <- function(libname, pkgname) {

  pkg.opt <- list(
    bigstatsr.ncores.max          = parallel::detectCores(),
    bigstatsr.check.parallel.blas = TRUE
  )

  toset <- !(names(pkg.opt) %in% names(.Options))
  if (any(toset)) options(pkg.opt[toset])
}

################################################################################

.onUnload <- function(libpath) {
  options(
    default.nproc.blas = NULL
  )
}

################################################################################
