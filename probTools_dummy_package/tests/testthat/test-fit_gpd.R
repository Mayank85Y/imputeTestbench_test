test_that("fit_gpd correctly fits data", {
  data <- rgamma(100, shape = 2, scale = 1)
  result <- fit_gpd(data)
  expect_named(result, c("shape", "scale", "summary"))
})
