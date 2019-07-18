.distr6 = list()
.distr6$message_numeric = "Results from numeric calculations are approximate only. Better results may be available."
.distr6$message_missing = "One of cdf or pdf must be provided."
.distr6$huberize_discrete  = "Only discrete and continuous distributions can be Huberized."
.distr6$missing_analytic = function(self,method)
  return(paste("No analytic result for",self$name,method,"available. Try decorating with CoreStatistics."))
