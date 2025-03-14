#' Bayesian Update with MAP Estimation
#'
#' Computes the posterior distribution using Bayes' theorem and finds the MAP estimate.
#'
#' @param prior Numeric vector of prior probabilities.
#' @param likelihood Numeric vector of likelihoods.
#' @return A list with posterior probabilities and MAP estimate.
#' @examples
#' prior <- c(0.2, 0.5, 0.3)
#' likelihood <- c(0.8, 0.1, 0.6)
#' bayesian_update(prior, likelihood)
#' @export
bayesian_update <- function(prior, likelihood) {
  posterior <- (prior * likelihood) / sum(prior * likelihood)
  map_estimate <- which.max(posterior)  # Maximum A Posteriori (MAP) estimate
  return(list(posterior = posterior, map_estimate = map_estimate))
}
