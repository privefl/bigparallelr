################################################################################

context("test-nb-cores")

################################################################################

test_that("nb_cores() works", {
  expect_lt(nbc <- nb_cores(), parallel::detectCores())
})

test_that("assert_cores() works", {

  options(bigstatsr.ncores.max = 1)
  expect_null(assert_cores(1))
  expect_error(assert_cores(2),   "You are trying to use more cores than allowed.")
  expect_error(assert_cores(2.5), "'ncores' should be an integer.")
  expect_error(assert_cores(0),   "'ncores' should be at least 1.")

  options(bigstatsr.ncores.max = Inf, bigstatsr.check.parallel.blas = TRUE)
  expect_null(assert_cores(1))

  set_blas_ncores(bigparallelr:::default_nproc_blas())
  if (get_blas_ncores() > 1) {
    if (nb_cores() > 1) {
      expect_error(assert_cores(nb_cores()),
                   "Two levels of parallelism are used.")
    }
    expect_error(assert_cores(parallel::detectCores() + 1),
                 "Two levels of parallelism are used.")
  } else {
    expect_null(assert_cores(nb_cores()))
    expect_null(assert_cores(parallel::detectCores() + 1))
  }

  options(bigstatsr.ncores.max = parallel::detectCores())
  expect_null(assert_cores(1))
  expect_error(assert_cores(parallel::detectCores() + 1),
               "You are trying to use more cores than allowed.")
})

################################################################################
