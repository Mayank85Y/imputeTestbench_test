#' Fit Generalized Pareto Distribution (GPD) with Summary Stats
#'
#' Fits a Generalized Pareto Distribution (GPD) to given data and provides summary statistics.
#'
#' @param data Numeric vector of data points.
#' @return A list containing the fitted distribution parameters and summary statistics.
#' @examples
#' data <- rgamma(100, shape = 2, scale = 1)
#' fit_gpd(data)
#' @importFrom stats var sd
#' @export
fit_gpd <- function(data) {
  fit <- evir::gpd(data, threshold = min(data))
  summary_stats <- list(mean = mean(data), variance = var(data), skewness = sum((data - mean(data))^3) / (length(data) * sd(data)^3))
  return(return(list(shape = fit$par.ests["xi"], scale = fit$par.ests["beta"], summary = fit)))
}
