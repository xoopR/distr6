#' @title Exotic Statistics Methods for Distributions
#'
#' @description Further functionality to distribution objects for statistical
#'   methods that can be considered more exotic than core, such as survival modelling
#'   and p-norms.
#' @name ExoticStatistics
#'
#' @section Usage: ExoticStatistics$new(distribution)
#' @return \code{ExoticStatistics$new} constructs an R6 object of class Distribution.
#'
#' @param distribution distribution object.
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by overwriting the object in the Global Environment. They can be specified
#'  in construction of the Distribution or by constructing the given Decorator.
#'
#'  Methods act on the distribution and not the constructor therefore method chaining of the form
#'  \code{ExoticStatistics$new(distribution)$hazard(1)} is not supported but \code{distribution$new(decorator=ExoticStatistics)$hazard(1)} is.
#'
#'
#' @seealso \code{\link{CoreStatistics}} for more available methods.
#'
#' @examples
#' \dontrun{
#' X = Binomial$new(decorator = "ExoticStatistics")
#' X$survival(1)
#' X$pdfPNorm()
#' }
#'
#' @examples
#' \dontrun{
#' X = Binomial$new()
#' ExoticStatistics$new(X)
#' X$pdfPNorm(4)
#' }
NULL


#' @export
ExoticStatistics <- R6::R6Class("ExoticStatistics", inherit = DistributionDecorator)
ExoticStatistics$set("public", "cdfAntiDeriv", function(lower = self$inf(),
                                                        upper = self$sup()){
  return(self$cdfPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)
ExoticStatistics$set("public", "survivalAntiDeriv", function(lower = self$inf(),
                                                             upper = self$sup()) {
  return(self$survivalPNorm(p = 1, lower, upper))
}) # NEEDS TESTING (p-norm)
ExoticStatistics$set("public", "logCdf", function(x) {
}) # TO DO
ExoticStatistics$set("public", "generalisedIntegral", function() {
}) # TO DO
ExoticStatistics$set("public", "survival", function(x, log.p=FALSE) {
  if(!log.p){
    return(1 - self$cdf(x))
  }
}) # IN PROGRESS
ExoticStatistics$set("public", "hazard", function(x, log=FALSE) {
  if(!log){
    return(self$pdf(x) / self$survival(x))
  }
}) # IN PROGRESS
ExoticStatistics$set("public", "cumHazard", function(x, log=FALSE) {
  if(!log){
    return(-log(self$survival(x)))
  }
}) # IN PROGRESS
ExoticStatistics$set("public", "generalPNorm", function(fun, p, lower, upper){
  if(testContinuous(self)){
    warning("Results from numerical integration are approximate only, better results may be available.")
    return((integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
  }
}) # NEEDS TESTING
ExoticStatistics$set("public", "cdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  return(self$generalPNorm(self$cdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "pdfPNorm", function(p = 2, lower = self$inf(),
                                                    upper = self$sup()) {
  return(self$generalPNorm(self$pdf, p, lower, upper))
}) # NEEDS TESTING
ExoticStatistics$set("public", "survivalPNorm", function(p = 2, lower = self$inf(),
                                                         upper = self$sup()) {
  return(self$generalPNorm(self$survival, p, lower, upper))
}) # NEEDS TESTING