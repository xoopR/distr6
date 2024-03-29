% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDistribution_Arrdist.R
\name{c.Arrdist}
\alias{c.Arrdist}
\title{Combine Array Distributions into a Arrdist}
\usage{
\method{c}{Arrdist}(..., decorators = NULL)
}
\arguments{
\item{...}{array distributions to be concatenated.}

\item{decorators}{If supplied then adds given decorators, otherwise pulls them from underlying distributions.}
}
\value{
\link{Arrdist}
}
\description{
Helper function for quickly combining distributions into a \link{Arrdist}.
}
\examples{
# create three array distributions with different column names
arr <- replicate(3, {
  pdf <- runif(400)
  arr <- array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
  arr <- aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  as.Distribution(arr, fun = "pdf")
})
do.call(c, arr)
}
