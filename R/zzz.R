R62S3::R62Fun(Distribution, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(SDistribution, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(Kernel, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(ParameterSet, assignEnvir = topenv())
R62S3::R62Fun(ExoticStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)
R62S3::R62Fun(CoreStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)
R62S3::R62Fun(DistributionWrapper, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n-------------------------------------------------")
  packageStartupMessage(" \tWelcome to distr6 v",utils::packageVersion("distr6"),
"\n\t------------------------
To get started see:
* ?distr6
* vignette('distr6','distr6')
* https://alan-turing-institute.github.io/distr6/")
  packageStartupMessage("-------------------------------------------------\n")
}
