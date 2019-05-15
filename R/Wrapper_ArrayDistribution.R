#' @name ArrayDistribution
#' @title Product ArrayDistribution
#' @description A special case joint distribution where each indepdent distribution is the same Distribution
#' but not necessarily with the same parameters.
#' @seealso \code{\link{ProductDistribution}} and \code{\link{Distribution}}. As well as \code{\link{DistributionWrapper}}
#' for wrapper details.
#' @details Exploits the following relationships of independent distributions
#' \deqn{f_P(x, y) = f_X(x) * f_Y(y)}
#' \deqn{F_P(x, y) = F_X(x) * F_Y(y)}
#' where f_P/F_P is the pdf/cdf of the joint (product) distribution P and X,Y are independent distributions.
#'
#' \code{JointDistribution} inherits all methods from \code{\link{Distribution}} and \code{\link{DistributionWrapper}}.
#'
#' @section Constructor Arguments:
#' \tabular{ll}{
#' \code{distribution} \tab Distribution. \cr
#' \code{paramVector} \tab vector of parameters, see details. \cr
#' }
#'
#' @examples
#' \dontrun{
#' prodBin <- ArrayDistribution$new(Binomial$new(prob = 0.5, size = 10),
#' Binomial$new(prob = 0.5, size = 20))
#' prodBin$pdf(2)
#' prodBin$cdf(5)
#' }
NULL

#' @export
ArrayDistribution <- R6::R6Class("ArrayDistribution", inherit = DistributionWrapper)
ArrayDistribution$set("public","initialize",function(distribution, paramVector){
  distribution = paste0(substitute(distribution))
  if(!(distribution %in% listDistributions(simplify = T)))
    stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

  distribution = get(distribution)$new()
  param_filter = unlist(distribution$parameters(as.df = T)["settable"])
  ids = distribution$parameters(as.df = T)[param_filter,"id"]
  lowers = distribution$parameters(as.df = T)[param_filter,"lower"]
  uppers = distribution$parameters(as.df = T)[param_filter,"upper"]
  classes = distribution$parameters(as.df = T)[param_filter,"class"]

  params = sapply(paramVector, function(x){
    checkmate::assert(length(x)==length(ids))
    for(i in 1:length(x)){
      index = which(ids %in% names(x)[[i]])
      x[[i]] = as(x[[i]], classes[index])
      checkmate::assertNumber(x[[i]], lower = lowers[index], upper = uppers[index])
    }
    return(x)
  })
  rownames(params) = 1:nrow(params)

})
