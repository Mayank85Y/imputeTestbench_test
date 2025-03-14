#' Custom Probability Distribution
#'
#' Computes the PDF, CDF, and generates random samples from a custom distribution.
#'
#' @param x Numeric vector of values.
#' @param alpha Shape parameter (default: 2).
#' @param beta Scale parameter (default: 3).
#' @param n_samples Number of random samples to generate (default: 1000).
#' @return A list with PDF, CDF, and random samples.
#' @examples
#' result <- custom_dis(1:10)
#' result$samples
#' @importFrom stats runif
#' @export
custom_dis <- function(x, alpha = 2, beta = 3, n_samples = 1000) {
  pdf <- alpha * beta * x^(alpha - 1) * exp(-beta * x^alpha)
  cdf <- 1 - exp(-beta * x^alpha)
  samples <- (-log(runif(n_samples)) / beta)^(1 / alpha)  # Random sampling using inverse transform
  return(list(pdf = pdf, cdf = cdf, samples = samples))
}
