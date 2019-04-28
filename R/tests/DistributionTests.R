dbin = function(x, log,...){
 m1 = choose(self$getParameterValue(id="size"), x)
 m2 = self$getParameterValue(id="prob")^x
 m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
 return(m1 * m2 * m3)
}
discreteTester = Distribution$new("TestDistr","Discrete Test",support=interval$new(0,100),
                          symmetric=T, type = posNaturals$new(),
                          distrDomain=posNaturals$new(),
                          pdf = dbin,
                          parameters = list(list(id = "prob",
                                                 name = "Probability of Success",
                                                 default = 0.5,
                                                 value = 0.2,
                                                 settable = TRUE,
                                                 fittable = TRUE,
                                                 class = "numeric",
                                                 lower = 0,
                                                 upper = 1,
                                                 description = "None"),
                                            list(id = "size",
                                                 name = "Number of trials",
                                                 default = 10,
                                                 settable = TRUE,
                                                 fittable = TRUE,
                                                 class = "integer",
                                                 lower = 0,
                                                 upper = Inf,
                                                 description = "None")),
                          decorators = list(CoreStatistics),
                          paramvalues = list(size = 100)
                          )

discreteTester$setParameterValue(list(prob = 0.9, size = 2))
discreteTester$kurtosis(); discreteTester$properties()$kurtosis
discreteTester$kurtosisType()
discreteTester$skewness(); discreteTester$skewnessType()
discreteTester$setParameterValue(list(prob = 0.1))
discreteTester$kurtosis(); discreteTester$properties()$kurtosis
discreteTester$kurtosisType()
discreteTester$skewness(); discreteTester$skewnessType()
discreteTester$properties()

discreteTester$mode()
discreteTester$expectation()
discreteTester$var()
discreteTester$kthmoment(2)
discreteTester$parameters()
discreteTester$pgf(z=2)
discreteTester$entropy()

discreteTester$kurtosis()
discreteTester$kthmoment(2)
discreteTester$kthmoment(3, type = "standard")
discreteTester$skewness()
discreteTester$kurtosis(F)
discreteTester$kurtosisType()

discreteTester2 = discreteTester$clone()
discreteTester2$convolution(discreteTester)(1:3); dbinom(x = 1:3, size = 4, prob = 0.1)
discreteTester2$convolution(discreteTester, add = F)(1:3)
