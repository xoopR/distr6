.distr6 <- list()
.distr6$message_numeric <-
  "Results from numeric calculations are approximate only. Better results may be available."
.distr6$message_missing <- "One of cdf or pdf must be provided."
.distr6$huberize_discrete <- "Only discrete and continuous distributions can be Huberized."
.distr6$missing_analytic <- function(self, method) {
  stopf(
    "No analytic result for %s %s available. Try decorating with CoreStatistics.",
    self$name, method
  )
}

.distr6$distributions <- data.table(
  ShortName = NULL, ClassName = NULL, Type = NULL,
  ValueSupport = NULL, VariateForm = NULL, Package = NULL,
  Tags = NULL
)
.distr6$kernels <- data.table(ShortName = NULL, ClassName = NULL, Support = NULL, Packages = NULL)


.distr6$decorators <- list()
.distr6$wrappers <- list()
