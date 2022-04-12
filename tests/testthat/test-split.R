################################################################################

context("test-split")


test_that("Sequence generation works", {

  mat <- matrix(1:18, 3, 6)

  expect_identical(rows_along(mat), 1:3)
  expect_identical(cols_along(mat), 1:6)

  expect_identical(seq_range(c(3, 10)), 3:10)
})


test_that("split_len() works", {

  expect_error(split_len(0, nb_split = 2),
               "'total_len' should have only positive values.")
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


test_that("split_vec() and split_df() work", {

  expect_equal(split_vec(1:10, block_len = 3), list(1:2, 3:5, 6:8, 9:10))
  expect_equal(split_vec(1:10, block_len = Inf), list(1:10))
  expect_equal(split_vec(1:10, nb_split = Inf), as.list(1:10))

  expect_error(split_df(iris[0, ], nb_split = 2),
               "'total_len' should have only positive values.")
  expect_equal(split_df(iris, nb_split = 3), unname(split(iris, iris$Species)))
})


test_that("split_costs() works", {

  replicate(200, {

    N <- sample(10^(2:4), 1)
    lambda <- sample(30, 1)
    costs <- rpois(N, lambda)
    nb <- sample(c(1, 2, 5, 10, 100, 1000), 1)

    res <- split_costs(costs, nb)

    res.costs <- res[, "cost"]
    expect_equal(sum(res.costs), sum(costs))
    if (N > 100 && nb == 10)
      expect_lt(sd(res.costs) / mean(res.costs), 0.1)

    intervals_seq <- lapply(rows_along(res), function(i) seq_range(res[i, ]))
    expect_equal(unlist(intervals_seq), 1:N)
  })
})
