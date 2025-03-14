#' Shannon Entropy with Normalized Output
#'
#' Computes Shannon entropy and normalizes it to a 0-1 scale.
#'
#' @param p Numeric vector of probabilities.
#' @return A list with Shannon entropy and normalized entropy.
#' @examples
#' probs <- c(0.2, 0.3, 0.5)
#' shannon_entropy(probs)
#' @export
shannon_entropy <- function(p) {
  p <- p[p > 0 ] #removing 0 prob to avoid log(0)
  entropy <- -sum(p * log2(p))
  normalized_entropy <- entropy / log2(length(p))  # Normalize entropy (0 to 1)
  return(list(entropy = entropy, normalized_entropy = normalized_entropy))
}


