library(testthat)

context("Function Imputation")

dexpo = function(x){
  m1 = self$getParameterValue("rate")
  m2 = exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}

ps = ParameterSet$new(id = list("rate", "scale"), value = list(1, 1),
                      lower = list(0, 0), upper = list(Inf, Inf),
                      class = list("numeric","numeric"),
                      settable = list(TRUE, FALSE),
                      fittable = list(TRUE, FALSE),
                      updateFunc = list(NULL, "1/self$getParameterValue('rate')"),
                      description = list("Arrival rate","Scale parameter"))

continuousTester = Distribution$new("Continuous Test","ContTest",support=PosReals$new(),
                                    symmetric=TRUE, type = PosReals$new(zero=T),
                                    distrDomain=PosReals$new(),
                                    pdf = dexpo,
                                    parameters = ps,
                                    R62S3 = FALSE
)

test_that("r/d/p/q null",{
  expect_silent(continuousTester$pdf(1))
  expect_null(continuousTester$cdf(1))
  expect_null(continuousTester$quantile(1))
  expect_null(continuousTester$rand(1))
})

test_that("r/d/p/q not null",{
  decorate(continuousTester, FunctionImputation, R62S3 = F)
  expect_silent(continuousTester$pdf(1))
  expect_message(continuousTester$cdf(1))
  expect_message(continuousTester$quantile(1))
  expect_message(continuousTester$rand(1))
})
