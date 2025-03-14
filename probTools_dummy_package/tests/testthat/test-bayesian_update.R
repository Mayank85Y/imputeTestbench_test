test_that("bayesian_update computes valid posterior", {
  prior <- c(0.2, 0.5, 0.3)
  likelihood <- c(0.8, 0.1, 0.6)
  result <- bayesian_update(prior, likelihood)
  expect_length(result$posterior, 3)
  expect_equal(sum(result$posterior), 1, tolerance = 1e-6)
})
