R62S3::R62Fun(Distribution, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(SDistribution, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(Kernel, assignEnvir = topenv(), mask = T)
R62S3::R62Fun(ParameterSet, assignEnvir = topenv())
R62S3::R62Fun(ExoticStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)
R62S3::R62Fun(CoreStatistics, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)
R62S3::R62Fun(DistributionWrapper, assignEnvir = topenv(), dispatchClasses = list(Distribution), mask = T)


.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("distr", "attach"), function(...) {
    packageStartupMessage("-----------------------------------------------------------------------")
    packageStartupMessage(
      "Loading distr and distr6 can cause problems. If you do require both then we recommend first loading distr and then distr6: \n
      library(distr); library(distr6)"
    )
    packageStartupMessage("-----------------------------------------------------------------------")
  })

  packageStartupMessage("\n-------------------------------------------------")
  packageStartupMessage(" \t\tWelcome to distr6. \n \t\t---------------- \n To get started see: \n * ?distr6 \n * vignette('distr6','distr6') \n * https://alan-turing-institute.github.io/distr6/")
  packageStartupMessage("-------------------------------------------------\n")
}
