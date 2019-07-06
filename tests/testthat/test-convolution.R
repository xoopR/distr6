library(testthat)

context("Convolution")

dexpo = function(x){
  m1 = self$getParameterValue("rate")
  m2 = exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}
cexpo = function(x){
  m1 = exp(-1 * self$getParameterValue("rate") * x)
  return(1 - m1)
}

ps = ParameterSet$new(id = list("rate", "scale","test"), value = list(1, 1, 0),
                      support = list(PosReals$new(zero = T), PosReals$new(zero = T), Interval$new(0,5)),
                      settable = list(TRUE, FALSE, FALSE),
                      updateFunc = list(NULL, "1/self$getParameterValue('rate')",
                                        "exp(self$getParameterValue('rate'))"),
                      description = list("Arrival rate","Scale parameter","testpar"))

continuousTester = Distribution$new("Continuous Test","ContTest",support=PosReals$new(),
                                    symmetric=TRUE, type = PosReals$new(zero=T),
                                    distrDomain=PosReals$new(),
                                    pdf = dexpo,
                                    cdf = cexpo,
                                    parameters = ps
)


dbin = function(x){
  m1 = choose(self$getParameterValue(id="size"), x)
  m2 = self$getParameterValue(id="prob")^x
  m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
  return(m1 * m2 * m3)
}

ps = ParameterSet$new(id = list("prob","size","qprob"), value = list(0.2, 100, 0.8),
                      support = list(Interval$new(0,1), PosNaturals$new(), Interval$new(0,1)),
                      settable = list(TRUE, TRUE, FALSE),
                      updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                      description = list("Probability of Success", "Number of trials",
                                         "Probability of failure"))

discreteTester = Distribution$new("Discrete Test","TestDistr",support=Set$new(0:10),
                                  symmetric=TRUE, type = PosNaturals$new(),
                                  distrDomain=PosNaturals$new(),
                                  pdf = dbin,
                                  parameters = ps,
                                  decorators = list(CoreStatistics)
)

test_that("check continuous convolution functions", {
  Exp1 = Exponential$new(rate = 1)
  Exp2 = Exponential$new(rate = 1)
  ConvE12 = Convolution$new(Exp1, Exp2)
  decorate(ConvE12, CoreStatistics)
  expect_equal(ConvE12$pdf(1), dgamma(x = 1, shape = 2))
  expect_equal(ConvE12$mean(), 2)
  expect_equal(ConvE12$variance(), 2)
  expect_equal(ConvE12$mean(), Exp1$mean() + Exp2$mean())
  expect_equal(ConvE12$variance(), Exp1$variance() + Exp2$variance())
})

test_that("check discrete convolution functions",{
  Bern1 = Bernoulli$new(prob = 0.1)
  Bern2 = Bernoulli$new(prob = 0.1)
  ConvB12 = Bern1 + Bern2
  decorate(ConvB12, CoreStatistics)
  expect_equal(ConvB12$pdf(1), dbinom(x = 1, size = 2, prob = 0.1))
  expect_equal(ConvB12$mean(), Bern1$mean() + Bern2$mean())
  expect_equal(ConvB12$variance(), Bern1$variance() + Bern1$variance())
})
