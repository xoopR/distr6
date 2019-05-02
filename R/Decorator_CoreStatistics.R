CoreStatistics <- R6::R6Class("CoreStatistics", inherit = DistributionDecorator)
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
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = getWorkingSupport(self)
    probs = self$pdf(rng)
    logs = log(self$pdf(rng), base)
    return(-sum(probs * logs))
  } else if(testContinuous(self)){
    warning("Results from numerical integration are approximate only, better results may be available.")
    return(-integrate(function(x) {
      probs = self$pdf(x)
      logs = log(self$pdf(x), base)
      logs[probs==0] = 0
      return(probs * logs)
    }, lower = self$inf(), upper = self$sup())$value)
  }
}) # DONE
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