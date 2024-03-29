% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dparse.R
\name{dparse}
\alias{dparse}
\title{Parse Distributions Represented as Strings}
\usage{
dparse(toparse)
}
\arguments{
\item{toparse}{\code{(character(1))}\cr
String to parse, which should be in the format Distribution([params]), see examples.}
}
\value{
Returns an R6 \link{Distribution}
}
\description{
Parse a custom string that represents an R6 distribution
}
\details{
Transform a custom (user) input to a R6 object.

This function is specially useful when you expect a user input which should
result in specific distribution. The distribution name must be the
ShortName, ClassName or Alias listed in the package, which can be found with
\code{\link[=listDistributions]{listDistributions()}}.
}
\examples{
dparse("N()")
dparse("norm(0, sd = 2)")
# lower and upper case work
dparse("n(sd = 1, mean = 4)")
dparse("T(df = 4)")
dparse("cHiSq(df = 3)")
# be careful to escape strings properly
dparse("C(list('A', 'B'), c(0.5, 0.5))")
dparse("Cat(elements = c('A', 'B'), probs = c(0.5, 0.5))")
}
