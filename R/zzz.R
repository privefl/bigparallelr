################################################################################

.onLoad <- function(libname, pkgname) {
  options(
    bigstatsr.ncores.max = parallel::detectCores()
  )
}

################################################################################

.onUnload <- function(libpath) {
  options(
    bigstatsr.ncores.max = NULL
  )
}

################################################################################
