test_that("order_stat_expectation returns reasonable values", {
  result <- order_stat_expectation(100, 5, rnorm, mean = 0, sd = 1)
  expect_length(result, 2)
})
