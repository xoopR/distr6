#' distr6: Object Oriented Distributions in R
#'
#' distr6 is an object oriented (OO) interface, primarily used for interacting with probability
#' distributions in R. Additionally distr6 includes functionality for composite distributions,
#' a symbolic representation for mathematical sets and intervals, basic methods for common kernels
#' and numeric methods for distribution analysis. distr6 is the official R6 upgrade to the distr
#' family of packages.
#'
#' The main features of distr6 are:
#'
#' \itemize{
#' \item Currently implements 29 probability distributions including all distributions in the R
#' stats package. Each distribution has (where possible) closed form analytic expressions for
#' basic statistical methods.
#' \item Decorators that add further functionality to probability distributions including numeric
#' results for useful modelling functions such as p-norms and k-moments.
#' \item Wrappers for composite distributions including convolutions, truncation, mixture
#' distributions and product distributions.
#' }
#'
#' To learn more about distr6, start with the distr6 vignette:
#'
#' \code{vignette("distr6", "distr6")}
#'
#' And for more advanced usage see the complete tutorials at
#'
#'\href{https://alan-turing-institute.github.io/distr6/index.html}{https://alan-turing-institute.github.io/distr6/index.html}
#'
"_PACKAGE"
