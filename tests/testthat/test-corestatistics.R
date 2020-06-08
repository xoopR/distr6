library(testthat)

dexpo <- function(x) {
  m1 <- self$getParameterValue("rate")
  m2 <- exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}
cexpo <- function(x) {
  m1 <- exp(-1 * self$getParameterValue("rate") * x)
  return(1 - m1)
}

ps <- ParameterSet$new(
  id = list("rate", "scale"), value = list(1, 1),
  support = list(PosReals$new(zero = T), PosReals$new(zero = T)),
  settable = list(TRUE, FALSE),
  updateFunc = list(
    NULL,
    function(self) 1 / self$getParameterValue("rate")
  ),
  description = list("Arrival rate", "Scale parameter")
)

continuousTester <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  pdf = dexpo, cdf = cexpo,
  parameters = ps,
  decorators = "CoreStatistics"
)

dbin <- function(x) {
  m1 <- choose(self$getParameterValue(id = "size"), x)
  m2 <- self$getParameterValue(id = "prob")^x
  m3 <- (1 - self$getParameterValue(id = "prob"))^(self$getParameterValue(id = "size") - x)
  return(m1 * m2 * m3)
}

ps <- ParameterSet$new(
  id = list("prob", "size", "qprob"), value = list(0.5, 10, 0.5),
  support = list(Interval$new(0, 1), Naturals$new(), Interval$new(0, 1)),
  settable = list(TRUE, TRUE, FALSE),
  updateFunc = list(
    NULL, NULL,
    function(self) 1 - self$getParameterValue("prob")
  ),
  description = list(
    "Probability of Success", "Number of trials",
    "Probability of failure"
  )
)

discreteTester <- Distribution$new("Discrete Test", "TestDistr",
  support = Set$new(0:10),
  symmetric = TRUE, type = Naturals$new(),
  pdf = dbin,
  parameters = ps,
  decorators = "CoreStatistics"
)

test_that("mgf", {
  expect_message(expect_equal(continuousTester$mgf(0.4), Exponential$new()$mgf(0.4)))
  expect_equal(discreteTester$mgf(2), Binomial$new()$mgf(2))
})

test_that("cf", {
  expect_message(expect_equal(continuousTester$cf(2), Exponential$new()$cf(2)))
  expect_equal(discreteTester$cf(2), Binomial$new()$cf(2))
})

test_that("pgf", {
  expect_equal(continuousTester$pgf(2), NaN)
  expect_equal(discreteTester$pgf(2), Binomial$new()$pgf(2))
})

test_that("entropy", {
  expect_message(expect_equal(round(continuousTester$entropy(), 1), round(Exponential$new()$entropy()), 1))
  expect_message(expect_equal(round(discreteTester$entropy(3), 2), round(Binomial$new()$entropy(3), 2)))
})

test_that("skewness", {
  expect_message(expect_equal(continuousTester$skewness(), Exponential$new()$skewness()))
  expect_equal(discreteTester$skewness(), Binomial$new()$skewness())
})

test_that("kurtosis", {
  expect_message(expect_equal(continuousTester$kurtosis(), Exponential$new()$kurtosis()))
  expect_equal(discreteTester$kurtosis(), Binomial$new()$kurtosis())
})

test_that("variance", {
  expect_message(expect_equal(continuousTester$variance(), Exponential$new()$variance()))
  expect_equal(discreteTester$variance(), Binomial$new()$variance())
})

test_that("kthmoment", {
  expect_message(expect_equal(continuousTester$kthmoment(3, "s"), Exponential$new()$skewness()))
  expect_message(expect_equal(discreteTester$kthmoment(3, "s"), Binomial$new()$skewness()))

  expect_message(expect_equal(continuousTester$kthmoment(3, "c"), 2))
  expect_message(expect_equal(discreteTester$kthmoment(3, "c"), 0))

  expect_message(expect_equal(continuousTester$kthmoment(3, "r"), 6))
  expect_message(expect_equal(discreteTester$kthmoment(3, "r"), 162.5))
})

test_that("mode", {
  expect_equal(discreteTester$mode(), Binomial$new()$mode())
  expect_equal(round(continuousTester$mode(), 3), round(Exponential$new()$mode(), 3))
})

test_that("mean", {
  expect_message(expect_equal(continuousTester$mean(), Exponential$new()$mean()))
  expect_equal(discreteTester$mean(), Binomial$new()$mean())
})


rbin <- function(n) {
  pdf <- function(x) {
    m1 <- choose(self$getParameterValue(id = "size"), x)
    m2 <- self$getParameterValue(id = "prob")^x
    m3 <- (1 - self$getParameterValue(id = "prob"))^(self$getParameterValue(id = "size") - x)
    return(m1 * m2 * m3)
  }
  return(sample(1:10, size = n, replace = T, prob = pdf(1:10)))
}
discreteTester <- Distribution$new("Discrete Test", "TestDistr",
  support = Set$new(0:10),
  symmetric = TRUE, type = Naturals$new(),
  pdf = dbin, rand = rbin,
  parameters = ps,
  decorators = "CoreStatistics"
)

test_that("rand2mode", {
  expect_equal(discreteTester$mode(), Binomial$new()$mode())
})
