#' @name ArrayDistribution-deprecated
#' @title Product Array Distribution
#'
#' @section Constructor: ArrayDistribution$new(distribution, paramList, name = NULL, short_name = NULL,
#' description = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to wrap. \cr
#' \code{paramList} \tab list \tab List of parameters, see example. \cr
#' \code{name} \tab list \tab Optional new name for distribution. \cr
#' \code{short_name} \tab list \tab Optional new short_name for distribution. \cr
#' \code{description} \tab list \tab Optional new description for distribution. \cr
#' }
#'
#' @description A special case product distribution where each independent distribution is the same
#' Distribution class but not necessarily with the same parameters.
#'
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_A(X1 = x1,...,XN = xN) = f_{X1}(x1) * ... * f_{XN}(xn)}
#' \deqn{F_A(X1 = x1,...,XN = xN) = F_{X1}(x1) * ... * F_{XN}(xn)}
#' where \eqn{f_A}/\eqn{F_A} is the pdf/cdf of the array distribution \eqn{A} and \eqn{X1,...,XN} are independent distributions.
#'
#' @seealso \code{\link{distr6-deprecated}}
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @return Returns an R6 object of class ArrayDistribution.
#'
#' @examples
#' a = ArrayDistribution$new(Binomial,
#'               list(list(prob = 0.1, size = 2),
#'                    list(prob = 0.6, size = 4),
#'                    list(prob = 0.2, size = 6)))
#' a$pdf(x1=1,x2=2,x3=3)
#' a$cdf(x1=1,x2=2,x3=3)
#' a$rand(10)
#'
#' @keywords internal
NULL


#' @name ArrayDistribution
#' @rdname distr6-deprecated
#' @export
NULL
ArrayDistribution <- R6Class("ArrayDistribution", inherit = ProductDistribution)
ArrayDistribution$set("public","initialize",function(...){
  .Deprecated("ProductDistribution", "distr6", "The ArrayDistribution wrapper is now deprecated and has
been merged with the ProductDistribution wrapper.")
})
