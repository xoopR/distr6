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
                          pdf = distribution$pdf,
                          cdf = distribution$cdf,
                          quantile = distribution$quantile,
                          rand = distribution$rand,
                          parameters = as.list(distribution$.__enclos_env__$private$.parameters),
                          decorators = decorators,
                          valueSupport = distribution$valueSupport(),
                          variateForm = distribution$variateForm(),
                          description = distribution$description()
         ), pos = .GlobalEnv)

  cat(paste(substitute(distribution),"is now decorated with",
            getR6Class(self),"\n"))
})

CoreStatistics <- R6Class("CoreStatistics", inherit = DistributionDecorator)
CoreStatistics$set("public", "mgf", function(x) {}) # TO DO
CoreStatistics$set("public", "cf", function(x) {}) # TO DO
CoreStatistics$set("public", "pgf", function(x) {}) # TO DO
CoreStatistics$set("public", "mad", function() {}) # TO DO
CoreStatistics$set("public", "iqr", function() {}) # TO DO
CoreStatistics$set("public", "fisher", function() {}) # TO DO
CoreStatistics$set("public", "entropy", function(base = 2) {}) # TO DO
CoreStatistics$set("public", "scale", function() {}) # TO DO
CoreStatistics$set("public", "skewness", function() {}) # TO DO
CoreStatistics$set("public", "skewnessType", function() {
  if(skewness() < 0)
    return("Negative Skew")
  else if(skewness() == 0)
    return("No Skew")
  else
    return("Positive Skew")
}) # DONE
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {}) # TO DO
CoreStatistics$set("public", "kurtosisType", function() {
  if(kurtosis() < 0)
    return("Platykurtic")
  else if(kurtosis() == 0)
    return("Mesokurtic")
  else
    return("Leptokurtic")
}) # DONE

ExoticStatistics <- R6Class("ExoticStatistics", inherit = DistributionDecorator)
ExoticStatistics$set("public", "cdfAntiDeriv", function(x, lower, upper) {}) # TO DO
ExoticStatistics$set("public", "cdf2Norm", function(x, lower, upper) {}) # TO DO
ExoticStatistics$set("public", "pdf2Norm", function(x, lower, upper) {}) # TO DO
ExoticStatistics$set("public", "logCdf", function(x) {}) # TO DO
ExoticStatistics$set("public", "generalisedIntegral", function() {}) # TO DO
ExoticStatistics$set("public", "survival", function(x, log=FALSE) {}) # TO DO
ExoticStatistics$set("public", "hazard", function(x, log=FALSE) {}) # TO DO
ExoticStatistics$set("public", "cumHazard", function(x, log=FALSE) {}) # TO DO
ExoticStatistics$set("public", "survival2Norm", function(x, lower, upper) {}) # TO DO


