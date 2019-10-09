################################################################################

context("test-register-parallel")

################################################################################

test_that("register_parallel() works", {

  test <- function(ncores, ...) {
    register_parallel(ncores, ...)
    foreach(i = 1:2) %dopar% i
  }

  expect_identical(test(1), as.list(1:2))
  expect_identical(test(2), as.list(1:2))
  expect_error({ foreach(i = 1:2) %dopar% i }, "invalid connection")
})

################################################################################
