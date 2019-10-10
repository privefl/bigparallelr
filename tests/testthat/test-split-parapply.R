################################################################################

context("test-split-parapply")

################################################################################

# Simulating some data
N <- 73
M <- 4300
x <- matrix(rnorm(N * M, mean = 100, sd = 5), N)

################################################################################

test_that("equality with other functions", {

  # get the means of each column (not combined)
  colmeans_split <- split_parapply(
    FUN = function(X, ind) colMeans(X[, ind, drop = FALSE]),
    ind = cols_along(x),
    X = x,
    # .combine = "c",
    ncores = 2
  )
  expect_length(colmeans_split, 2)
  expect_equal(unlist(colmeans_split), colMeans(x))

  # get the means of each column
  colmeans <- split_parapply(
    FUN = function(X, ind) colMeans(X[, ind, drop = FALSE]),
    ind = cols_along(x),
    X = x,
    .combine = "c",
    ncores = 2
  )
  expect_equal(colmeans, colMeans(x))

  # get the norms of each column
  colnorms <- split_parapply(
    FUN = function(X, ind) sqrt(colSums(X[, ind]^2)),
    ind = cols_along(x),
    X = x,
    .combine = "c",
    ncores = 2
  )
  expect_equal(colnorms, sqrt(colSums(x^2)))

  # get the sums of each row
  rowsums <- split_parapply(
    FUN = function(X, ind) rowSums(X[, ind]),
    ind = cols_along(x),
    X = x,
    .combine = "plus",
    ncores = 2
  )
  expect_equal(rowsums, rowSums(x))

  # get the maximum element of X (in absolute value)
  maxabs <- max(split_parapply(
    FUN = function(X, ind) max(abs(X[, ind])),
    ind = cols_along(x),
    X = x,
    .combine = "c",
    ncores = 2
  ))
  expect_equal(maxabs, max(abs(x)))

})

################################################################################

