library(R6)

DistributionDecorator <- R6Class("DistributionDecorator")
DistributionDecorator$set("public","initialize",function(distribution){
  if(getR6Class(self) == "DistributionDecorator")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  decorators = distribution$decorators()
  if(!is.null(decorators)){
    decorators = lapply(decorators,get)
  }
  decorators = unique(c(decorators,get(getR6Class(self))))

  assign(paste0(substitute(distribution)),
         Distribution$new(name = distribution$name(),
                          short_name = distribution$short_name(),
                          type = distribution$type(),
                          support = distribution$support(),
                          distrDomain = distribution$distrDomain(),
                          symmetric = as.logical(distribution$symmetry()),
                          pdf = distribution$.__enclos_env__$private$.pdf,
                          cdf = distribution$.__enclos_env__$private$.cdf,
                          quantile = distribution$.__enclos_env__$private$.quantile,
                          rand = distribution$.__enclos_env__$private$.rand,
                          parameters = distribution$.__enclos_env__$private$.parameters,
                          decorators = decorators,
                          valueSupport = distribution$valueSupport(),
                          variateForm = distribution$variateForm(),
                          description = distribution$description()
         ), pos = .GlobalEnv)

  cat(paste(substitute(distribution),"is now decorated with",
            getR6Class(self),"\n"))
})

CoreStatistics <- R6Class("CoreStatistics", inherit = DistributionDecorator)
CoreStatistics$set("public", "mgf", function(t) {
  return(self$expectation(trafo = function(x) {return(exp(x*t))}))
}) # DONE
CoreStatistics$set("public", "cf", function(t) {
}) # TO DO
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$expectation(trafo = function(x) {return(z^x)})
    return(x)
  }
}) # DONE
CoreStatistics$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
}) # DONE
CoreStatistics$set("public", "entropy", function(base = 2) {
  if(testDiscrete(self)){
    probs = self$pdf(self$support()$numeric())
    logs = log(self$pdf(self$support()$numeric()), base)
    return(-sum(probs * logs))
  } else if(testContinuous(self)){
    return(-integrate(function(x) {
      probs = self$pdf(x)
      logs = log(self$pdf(x), base)
      logs[probs==0] = 0
      return(probs * logs)
    }, lower = self$inf(), upper = self$sup())$value)
  }
}) # DONE
CoreStatistics$set("public", "scale", function() {
  return((self - self$expectation())/ self$sd())
}) # NEEDS TESTING ONCE ARITHS WRITTEN
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
}) # DONE
CoreStatistics$set("public", "skewnessType", function() {
  return(self$.__enclos_env__$private$.properties$skewness)
}) # DONE
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis = self$kthmoment(k = 4, type = "standard")
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
}) # DONE
CoreStatistics$set("public", "kurtosisType", function() {
  return(self$.__enclos_env__$private$.properties$kurtosis)
}) # DONE
CoreStatistics$set("public", "kthmoment", function(k, type = "central"){

  if(testUnivariate(self)){
    if(type == "central"){
      if(k == 0)
        return(1)
      if(k == 1)
        return(0)
    }

    centralMoment = self$expectation(trafo = function(x) return((x - self$expectation())^k))

    if(type == "central")
      return(centralMoment)
    else if(type == "standard")
      return(centralMoment / self$sd()^k)
  }
}) # DONE

ExoticStatistics <- R6Class("ExoticStatistics", inherit = DistributionDecorator)
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
    return((integrate(f = function(x) abs(fun(x))^p,lower,upper)$value)^(1/p))
  }
})
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


