library(testthat)

context("Function Imputation")

dexpo = function(x){
  m1 = self$getParameterValue("rate")
  m2 = exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}

cexpo = function(x){
  return(1 - exp(-self$getParameterValue("rate") * x))
}

ps = ParameterSet$new(id = list("rate", "scale","test"), value = list(1, 1, 0),
                      support = list(PosReals$new(zero = T), PosReals$new(zero = T), Interval$new(0,5)),
                      settable = list(TRUE, FALSE, FALSE),
                      updateFunc = list(NULL, "1/self$getParameterValue('rate')",
                                        "exp(self$getParameterValue('rate'))"),
                      description = list("Arrival rate","Scale parameter","testpar"))

continuousTester = Distribution$new("Continuous Test","ContTest",support=PosReals$new(),
                                    symmetric=TRUE, type = PosReals$new(zero=T),
                                    pdf = dexpo,
                                    parameters = ps
)

test_that("continuous r/d/p/q null d2",{
  expect_silent(continuousTester$pdf(1))
  expect_null(continuousTester$cdf(1))
  expect_null(continuousTester$quantile(1))
  expect_null(continuousTester$rand(1))
})

test_that("continuous r/p/q d2",{
  decorate(continuousTester, FunctionImputation)
  expect_silent(expect_equal(continuousTester$pdf(1),dexp(1)))
  expect_message(expect_equal(continuousTester$cdf(1),pexp(1)))
  expect_message(expect_equal(continuousTester$cdf(1:3),pexp(1:3)))
  expect_message(expect_equal(round(continuousTester$quantile(0.42),5), round(qexp(0.42), 5)))
  expect_message(continuousTester$rand(1))
})

continuousTester = Distribution$new("Continuous Test","ContTest",support=PosReals$new(),
                                    symmetric=TRUE, type = PosReals$new(zero=T),
                                    cdf = cexpo,
                                    parameters = ps
)

test_that("r/d/p/q null p2",{
  expect_null(continuousTester$pdf(1))
  expect_silent(continuousTester$cdf(1))
  expect_null(continuousTester$quantile(1))
  expect_null(continuousTester$rand(1))
})

test_that("r/d/p/q not null p2",{
  decorate(continuousTester, FunctionImputation)
  expect_message(expect_equal(continuousTester$pdf(1),dexp(1)))
  expect_silent(expect_equal(continuousTester$cdf(1),pexp(1)))
  expect_message(expect_equal(round(continuousTester$quantile(0.42),5), round(qexp(0.42), 5)))
  expect_message(expect_equal(round(continuousTester$quantile(c(0.42,0.24)),5), round(qexp(c(0.42,0.24)), 5)))
  expect_message(continuousTester$rand(1))
})


dbin = function(x){
  m1 = choose(self$getParameterValue(id="size"), x)
  m2 = self$getParameterValue(id="prob")^x
  m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x)
  return(m1 * m2 * m3)
}

pbin = function(x){
  sapply(x, function(x1){
    return(sum(sapply(0:x1, function(x1){
      m1 = choose(self$getParameterValue(id="size"), x1)
      m2 = self$getParameterValue(id="prob")^x1
      m3 = (1-self$getParameterValue(id="prob"))^(self$getParameterValue(id="size") - x1)
      return(m1 * m2 * m3)
    })))
  })

}

ps = ParameterSet$new(id = list("prob","size","qprob"), value = list(0.5, 10, 0.8),
                      support = list(Interval$new(0,1), PosNaturals$new(), Interval$new(0,1)),
                      settable = list(TRUE, TRUE, FALSE),
                      updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                      description = list("Probability of Success", "Number of trials",
                                         "Probability of failure"))

discreteTester = Distribution$new("Discrete Test","TestDistr",support=Set$new(0:10),
                                  symmetric=TRUE, type = PosNaturals$new(),
                                  pdf = dbin,
                                  parameters = ps,
                                  decorators = list(CoreStatistics)
)

test_that("discrete r/p/q",{
  decorate(discreteTester, FunctionImputation)
  expect_silent(expect_equal(discreteTester$pdf(1),dbinom(1,10,0.5)))
  expect_equal(discreteTester$cdf(1),pbinom(1,10,0.5))
  expect_message(expect_equal(discreteTester$quantile(0.42), qbinom(0.42,10, 0.5)))
  expect_message(discreteTester$rand(1))
})

discreteTester = Distribution$new("Discrete Test","TestDistr",support=Set$new(0:10),
                                  symmetric=TRUE, type = PosNaturals$new(),
                                  cdf = pbin,
                                  parameters = ps,
                                  decorators = list(FunctionImputation)
)

test_that("discrete r/p/q",{
  expect_silent(expect_equal(discreteTester$cdf(1),pbinom(1,10,0.5)))
  expect_equal(discreteTester$pdf(1),dbinom(1,10,0.5))
  expect_message(expect_equal(discreteTester$quantile(0.42), qbinom(0.42,10, 0.5)))
  expect_message(expect_equal(discreteTester$quantile(c(0.42,0.24)), qbinom(c(0.42,0.24), 10, 0.5)))
  expect_message(discreteTester$rand(1))
})

test_that("multivariate error",{
  expect_error(MultivariateNormal$new(decorators = FunctionImputation))
})

