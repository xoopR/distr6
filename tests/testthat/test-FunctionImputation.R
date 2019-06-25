library(testthat)

context("Function Imputation")

dexpo = function(x){
  m1 = self$getParameterValue("rate")
  m2 = exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
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
                                    parameters = ps,
)

test_that("r/d/p/q null",{
  expect_silent(continuousTester$pdf(1))
  expect_null(continuousTester$cdf(1))
  expect_null(continuousTester$quantile(1))
  expect_null(continuousTester$rand(1))
})

test_that("r/d/p/q not null",{
  decorate(continuousTester, FunctionImputation)
  expect_silent(expect_equal(continuousTester$pdf(1),dexp(1)))
  expect_message(expect_equal(continuousTester$cdf(1),pexp(1)))
  expect_message(expect_equal(round(continuousTester$quantile(0.42),5), round(qexp(0.42), 5)))
  expect_message(continuousTester$rand(1))
})
