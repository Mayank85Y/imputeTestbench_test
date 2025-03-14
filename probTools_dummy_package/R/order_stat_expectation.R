#' Order Statistic Expectation with Confidence Interval
#'
#' Estimates the expectation of an order statistic and provides a confidence interval.
#'
#' @param n Sample size.
#' @param k Order statistic index.
#' @param dist_func Function to generate random samples.
#' @param ... Additional arguments for the distribution function.
#' @param conf_level Confidence level for interval estimation (default: 0.95).
#' @return A list with the expected value and confidence interval.
#' @examples
#' order_stat_expectation(100, 5, rnorm, mean = 0, sd = 1)
#' @importFrom stats quantile
#' @export
order_stat_expectation <- function(n, k, dist_func, ..., conf_level = 0.95) {
  samples <- replicate(10000, sort(dist_func(n, ...))[k])
  mean_est <- mean(samples)
  conf_int <- quantile(samples, probs = c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2))
  return(list(mean = mean_est, conf_int = conf_int))
}
