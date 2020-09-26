#' @importFrom R62S3 R62Fun
#' @importFrom R6 R6Class
#' @importFrom data.table data.table as.data.table
#' @importFrom checkmate assert
#' @import set6
R62Fun(Distribution, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(SDistribution, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(Kernel, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(ParameterSet, assignEnvir = topenv(), scope = c("public"))
R62Fun(ExoticStatistics,
  assignEnvir = topenv(), dispatchClasses = list(Distribution),
  scope = c("public", "active")
)
R62Fun(CoreStatistics,
  assignEnvir = topenv(), dispatchClasses = list(Distribution),
  scope = c("public", "active")
)
R62Fun(DistributionWrapper,
  assignEnvir = topenv(), dispatchClasses = list(Distribution),
  scope = c("public", "active")
)
