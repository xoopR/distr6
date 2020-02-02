#' @importFrom R62S3 R62Fun
R62Fun(Distribution, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(SDistribution, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(Kernel, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(ParameterSet, assignEnvir = topenv(), scope = c("public", "active"))
R62Fun(ExoticStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), scope = c("public", "active"))
R62Fun(CoreStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), scope = c("public", "active"))
R62Fun(DistributionWrapper, assignEnvir = topenv(), dispatchClasses = list(Distribution), scope = c("public", "active"))


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n-----------------------------")
  packageStartupMessage("\tdistr6 v 1.3.4",
"\n\nGet started:\t?distr6
Changelog:\tdistr6News()")
  packageStartupMessage("-----------------------------\n")
}
