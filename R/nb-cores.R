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
