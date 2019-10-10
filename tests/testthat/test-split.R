################################################################################

context("test-split")

################################################################################

test_that("split_len() works", {
  expect_equal(split_len(10, block_len = 3),
               cbind(lower = c(1, 3, 6, 9),
                     upper = c(2, 5, 8, 10),
                     size = c(2, 3, 3, 2)))
  expect_equal(split_len(10, nb_split = 3),
               cbind(lower = c(1, 4, 8), upper = c(3, 7, 10), size = c(3, 4, 3)))
  expect_equal(split_len(10, block_len = 1),
               cbind(lower = 1:10, upper = 1:10, size = rep(1, 10)))
  expect_equal(split_len(10, block_len = 0),
               cbind(lower = 1:10, upper = 1:10, size = rep(1, 10)))
  expect_equal(split_len(10, block_len = 11),
               cbind(lower = 1, upper = 10, size = 10))
  expect_equal(split_len(10, nb_split = 11), split_len(10, block_len = 1))
  expect_equal(split_len(10, nb_split = 1), split_len(10, block_len = 10))
  expect_equal(split_len(10, nb_split = 0), split_len(10, block_len = 10))
})

################################################################################

test_that("Sequence generation works", {

  mat <- matrix(1:18, 3, 6)

  expect_identical(rows_along(mat), 1:3)
  expect_identical(cols_along(mat), 1:6)

  expect_identical(seq_range(c(3, 10)), 3:10)
})

################################################################################
