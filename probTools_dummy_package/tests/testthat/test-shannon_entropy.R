test_that("shannon_entropy produces expected results", {
  probs <- c(0.2, 0.3, 0.5)
  result <- shannon_entropy(probs)
  expect_length(result$entropy,1)
  expect_gte(result$entropy, 0)
})
