#' @name ArrayDistribution
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
#' \deqn{f_A(X1 = x1,...,XN = xN) = f_X1(x1) * ... * f_XN(xn)}
#' \deqn{F_A(X1 = x1,...,XN = xN) = F_X1(x1) * ... * F_XN(xn)}
#' where f_A/F_A is the pdf/cdf of the joint (product) distribution P and X1,...,XN are independent distributions.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
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
#' @export
NULL
ArrayDistribution <- R6::R6Class("ArrayDistribution", inherit = ProductDistribution)
ArrayDistribution$set("public","initialize",function(distribution, paramList, name = NULL,
                                                     short_name = NULL, description = NULL){
  distribution = paste0(substitute(distribution))
  if(!(distribution %in% listDistributions(simplify = T)))
    stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

  distribution = get(distribution)

  distlist = makeUniqueDistributions(sapply(paramList, function(x) do.call(distribution$new, x)))

  if(is.null(name)) name = paste0("Array",length(distlist),distribution$public_fields$name)
  if(is.null(short_name)) short_name = paste0("Array",length(distlist),distribution$public_fields$short_name)
  if(is.null(description)) description = paste0("Array of ",length(paramList)," ",distribution$public_fields$description)

  super$initialize(distlist, name, short_name, description)
})
