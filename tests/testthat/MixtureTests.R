library(testthat)

dexpo = function(x, log,...){
  m1 = self$getParameterValue("lambda")
  m2 = exp(-1 * self$getParameterValue("lambda") * x)
  return(m1 * m2)
}
cexpo = function(x, lower.tail = T, log.p = F,...){
  m1 = exp(-1 * self$getParameterValue("lambda") * x)
  return(1 - m1)
}
continuousTester = Distribution$new("Continuous Test","ContTest",support=posReals$new(),
                                    symmetric=TRUE, type = posReals$new(zero=T),
                                    distrDomain=posReals$new(),
                                    pdf = dexpo, cdf = cexpo,
                                    parameters = list(list(id = "lambda",
                                                           name = "Rate",
                                                           default = 1,
                                                           settable = TRUE,
                                                           fittable = TRUE,
                                                           class = "numeric",
                                                           lower = 0,
                                                           upper = Inf,
                                                           description = "None")))


dbin = function(x, log,...){
  m1 = choose(self$getParameterValue(id="size"), x)
  m2 = self$getParameterValue(id="prob")^x
  m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
  return(m1 * m2 * m3)
}
discreteTester = Distribution$new("Discrete Test","TestDistr",support=interval$new(0,100),
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
                                                         description = "None")))

test_that("check continuous Mixture wrapper", {
  continuousTester2 = continuousTester$clone()
  continuousTester2$.__enclos_env__$private$.short_name = "ContTest2"
  continuousTester3 = MixtureDistribution$new(list(continuousTester,continuousTester2))
  expect_equal(continuousTester3$pdf(1:10), continuousTester$pdf(1:10))
  expect_equal(continuousTester3$cdf(1:10), continuousTester$cdf(1:10))
  continuousTester2$setParameterValue(list(lambda = 0.1))
  continuousTester4 = MixtureDistribution$new(list(continuousTester,continuousTester2),
                                              weights = c(0.1,0.9))
})

test_that("check discrete mixture wrapper", {
  discreteTester2 = discreteTester$clone()
  discreteTester2$.__enclos_env__$private$.short_name = "DiscreteTest2"
  discreteTester3 = MixtureDistribution$new(list(discreteTester,discreteTester2))
  expect_equal(discreteTester3$pdf(1:10), discreteTester$pdf(1:10))
})
