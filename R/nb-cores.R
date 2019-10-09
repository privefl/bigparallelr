################################################################################

#' Check number of cores
#'
#' @param ncores Number of cores to check. Make sure is not larger than
#'   `getOption("bigstatsr.ncores.max")` (number of logical cores by default).
#'
#' @export
#'
#' @examples
#' assert_cores(2)
#'
assert_cores <- function(ncores) {
  if (ncores > getOption("bigstatsr.ncores.max")) {
    stop2(paste0(
      "You are trying to use more cores than allowed.",
      " We advise you to use `nb_cores()`.\n",
      "If you really know what you are doing, you can change this default value",
      " with `options(bigstatsr.ncores.max = Inf)`."
    ))
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
