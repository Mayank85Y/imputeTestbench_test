% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shannon_entropy.R
\name{shannon_entropy}
\alias{shannon_entropy}
\title{Shannon Entropy with Normalized Output}
\usage{
shannon_entropy(p)
}
\arguments{
\item{p}{Numeric vector of probabilities.}
}
\value{
A list with Shannon entropy and normalized entropy.
}
\description{
Computes Shannon entropy and normalizes it to a 0-1 scale.
}
\examples{
probs <- c(0.2, 0.3, 0.5)
shannon_entropy(probs)
}
