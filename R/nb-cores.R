################################################################################

default_nproc_blas <- function() {

  cl <- parallel::makePSOCKcluster(1)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterEvalQ(cl, RhpcBLASctl::blas_get_num_procs())[[1]]
}

################################################################################

#' Number of cores used by BLAS (matrix computations)
#'
#' @export
#'
#' @examples
#' get_blas_ncores()
get_blas_ncores <- function() {

  utils::capture.output({
    ncores <- RhpcBLASctl::blas_get_num_procs()
  })

  ncores
}

#' @rdname get_blas_ncores
#'
#' @param ncores Number of cores to set for BLAS.
#'
#' @export
#'
set_blas_ncores <- function(ncores) {

  save <- get_blas_ncores()

  utils::capture.output({
    RhpcBLASctl::blas_set_num_threads(ncores)
  })

  invisible(save)
}

################################################################################

#' Check number of cores
#'
#' Check that you are not trying to use too many cores.
#'
#' It also checks if two levels of parallelism are used, i.e. having `ncores`
#' larger than 1, and having a parallel BLAS enabled by default.
#' You could remove this check by setting
#' `options(bigstatsr.check.parallel.blas = FALSE)`.
#'
#' We instead recommend that you disable parallel BLAS by default by adding
#' `try(bigparallelr::set_blas_ncores(1), silent = TRUE)` to your .Rprofile
#' (**with an empty line at the end of this file**) so that this is set whenever
#' you start a new R session. You can use `usethis::edit_r_profile()` to open
#' your .Rprofile. For this to be effective, you should restart the R session or
#' run `options(default.nproc.blas = NULL)` once in the current session.
#'
#' Then, in a specific R session, you can set a different number of cores to use
#' for matrix computations using `bigparallelr::set_blas_ncores()`, if you know
#' there is no other level of parallelism involved in your code.
#'
#' @param ncores Number of cores to check. Make sure is not larger than
#'   `getOption("bigstatsr.ncores.max")` (number of logical cores by default).
#'   We advise you to use `nb_cores()`. If you really know what you are doing,
#'   you can change this default value with `options(bigstatsr.ncores.max = Inf)`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' assert_cores(2)
#' }
#'
assert_cores <- function(ncores) {

  if (ncores > getOption("bigstatsr.ncores.max"))
    stop2("You are trying to use more cores than allowed. See `?assert_cores`.")

  if (ncores > 1 && getOption("bigstatsr.check.parallel.blas")) {
    if (is.null(getOption("default.nproc.blas")))
      options(default.nproc.blas = default_nproc_blas())
    if (getOption("default.nproc.blas") > 1)
      stop2("Two levels of parallelism are used. See `?assert_cores`.")
  }
}

################################################################################

#' Recommended number of cores to use
#'
#' This is base on the following rule: use only physical cores and if you have
#' only physical cores, leave one core for the OS/UI.
#'
#' @return The recommended number of cores to use.
#' @export
#'
#' @examples
#' nb_cores()
nb_cores <- function() {

  if (Sys.info()[["sysname"]] == "Windows") {
    ncores <- parallel::detectCores(logical = FALSE)
  } else {
    # https://stackoverflow.com/a/23378780/6103040
    cmd <- "[ $(uname) = 'Darwin' ] && sysctl -n hw.physicalcpu_max ||
            lscpu -p | egrep -v '^#' | sort -u -t, -k 2,4 | wc -l"
    ncores <- as.integer(system(cmd, intern = TRUE))
  }

  all_cores <- parallel::detectCores(logical = TRUE)

  `if`(ncores < all_cores, ncores, all_cores - 1L)
}

################################################################################
