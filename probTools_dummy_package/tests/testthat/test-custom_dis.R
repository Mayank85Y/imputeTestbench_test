test_that("custom_dis works correctly", {
  result <- custom_dis(1:10)
  expect_length(result$pdf, 10)
  expect_length(result$cdf, 10)
  expect_gt(length(result$samples), 0)
})
