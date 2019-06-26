library(testthat)

context("Custom continuous distributions")

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

test_that("check all accessors are working", {
  expect_equal(continuousTester$decorators(), NULL)
  expect_equal(continuousTester$valueSupport(), "continuous")
  expect_equal(continuousTester$variateForm(), "univariate")
  expect_equal(continuousTester$symmetry(), "symmetric")
  expect_warning(continuousTester$getParameterValue("size"))
})

test_that("check core statistics", {
  expect_silent(continuousTester$setParameterValue(list(rate = 6)))
  expect_message(decorate(continuousTester, CoreStatistics))
  expect_equal(continuousTester$genExp(), 1/6)
  expect_equal(continuousTester$var(), 1/36)
  expect_equal(continuousTester$sd(), 1/6)
  expect_silent(continuousTester$iqr())
  expect_equal(continuousTester$kthmoment(0), 1)
  expect_equal(continuousTester$kthmoment(1), 0)
  expect_equal(continuousTester$kthmoment(2), continuousTester$var())
  expect_equal(continuousTester$kthmoment(3, type = "standard"), continuousTester$skewness())
  expect_equal(continuousTester$kthmoment(4, type = "standard"), continuousTester$kurtosis(FALSE))
  expect_message(continuousTester$setParameterValue(list(rate = 10)))
  expect_equal(continuousTester$mgf(6), continuousTester$getParameterValue("rate") / (continuousTester$getParameterValue("rate") - 6))
  expect_message(continuousTester$setParameterValue(list(rate = 5)))
  expect_equal(continuousTester$entropy(base=exp(1)), 1-log(continuousTester$getParameterValue("rate")))
})

test_that("check exotic statistics", {
  expect_message(decorate(continuousTester, ExoticStatistics))
  expect_equal(continuousTester$survival(1), 1-continuousTester$cdf(1))
  expect_equal(round(continuousTester$survivalAntiDeriv(), 5), round(continuousTester$survivalPNorm(p = 1), 5))
  expect_equal(round(continuousTester$genExp(), 5), round(continuousTester$survivalPNorm(p = 1), 5))
  expect_equal(continuousTester$hazard(3), continuousTester$pdf(3)/continuousTester$survival(3))
  expect_equal(-log(continuousTester$survival(3)), continuousTester$cumHazard(3))
})


