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
                                                         description = "None")),
                                  paramvalues = list(lambda = 6)
)

test_that("check all accessors are working", {
  expect_equal(continuousTester$decorators(), NULL)
  expect_equal(continuousTester$valueSupport(), "continuous")
  expect_equal(continuousTester$variateForm(), "univariate")
  expect_true(continuousTester$symmetry())
  expect_is(continuousTester$getParameterValue("size"), "character")
})

test_that("check basic maths and stats", {
  expect_equal(continuousTester$expectation(), 1/6)
  expect_equal(continuousTester$var(), 1/36)
  expect_equal(continuousTester$sd(), 1/6)
})

test_that("check core statistics", {
  expect_warning(CoreStatistics$new(continuousTester))
  expect_equal(continuousTester$kthmoment(2), continuousTester$var())
  expect_equal(continuousTester$kthmoment(3, type = "standard"), continuousTester$skewness())
  expect_equal(continuousTester$kthmoment(4, type = "standard"), continuousTester$kurtosis(FALSE))
  expect_warning(continuousTester$setParameterValue(list(lambda = 10)))
  expect_equal(continuousTester$mgf(6), continuousTester$getParameterValue("lambda") / (continuousTester$getParameterValue("lambda") - 6))
  expect_warning(continuousTester$setParameterValue(list(lambda = 5)))
  expect_equal(continuousTester$entropy(base=exp(1)), 1-log(continuousTester$getParameterValue("lambda")))
})

test_that("check exotic statistics", {
  expect_warning(ExoticStatistics$new(continuousTester))
  expect_equal(continuousTester$survival(1), 1-continuousTester$cdf(1))
  expect_equal(round(continuousTester$survivalAntiDeriv(), 5), round(continuousTester$survivalPNorm(p = 1), 5))
  expect_equal(round(continuousTester$expectation(), 5), round(continuousTester$survivalPNorm(p = 1), 5))
  expect_equal(continuousTester$hazard(3), continuousTester$pdf(3)/continuousTester$survival(3))
  expect_equal(-log(continuousTester$survival(3)), continuousTester$cumHazard(3))
})


