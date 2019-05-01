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