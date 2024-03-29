% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wrapper_VectorDistribution.R
\name{[.VectorDistribution}
\alias{[.VectorDistribution}
\title{Extract one or more Distributions from a VectorDistribution}
\usage{
\method{[}{VectorDistribution}(vecdist, i)
}
\arguments{
\item{vecdist}{VectorDistribution from which to extract Distributions.}

\item{i}{indices specifying distributions to extract or ids of wrapped distributions.}
}
\description{
Once a \code{VectorDistribution} has been constructed, use \code{[}
to extract one or more \code{Distribution}s from inside it.
}
\examples{
v <- VectorDistribution$new(distribution = "Binom", params = data.frame(size = 1:2, prob = 0.5))
v[1]
v["Binom1"]

}
