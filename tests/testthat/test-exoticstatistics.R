library(testthat)

context("Exotic Statistics")

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
                                    parameters = ps,
                                    decorators = ExoticStatistics
)

test_that("numeric survival functions",{
  expect_message(continuousTester$survival(1))
  expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F))
  expect_message(continuousTester$hazard(1))
  expect_equal(continuousTester$hazard(1), 1)
  expect_equal(continuousTester$hazard(1, T), 0)
})

continuousTester = Distribution$new("Continuous Test","ContTest",support=PosReals$new(),
                                    symmetric=TRUE, type = PosReals$new(zero=T),
                                    distrDomain=PosReals$new(),
                                    pdf = dexpo, cdf = cexpo,
                                    parameters = ps,
                                    decorators = ExoticStatistics
)

test_that("analytic survival functions",{
  expect_silent(continuousTester$survival(1))
  expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F))
  expect_silent(continuousTester$hazard(1))
  expect_equal(continuousTester$hazard(1), 1)
  expect_equal(continuousTester$hazard(1, T), 0)
})

