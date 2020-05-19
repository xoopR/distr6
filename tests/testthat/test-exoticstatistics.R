library(testthat)

context("Exotic Statistics")

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
  pdf = dexpo,
  parameters = ps,
  decorators = ExoticStatistics
)

test_that("numeric survival functions", {
  expect_message(expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F)))
  expect_message(expect_equal(continuousTester$hazard(1), dexp(1) / pexp(1, lower.tail = F)))
  expect_message(expect_equal(continuousTester$hazard(1, T), log(dexp(1) / pexp(1, lower.tail = F))))
  expect_message(expect_equal(continuousTester$cumHazard(2), -pexp(2, log = T, lower.tail = F)))
})

continuousTester <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  cdf = cexpo,
  parameters = ps,
  decorators = ExoticStatistics
)

test_that("numeric survival functions", {
  expect_silent(expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F)))
  expect_message(expect_equal(continuousTester$hazard(1), dexp(1) / pexp(1, lower.tail = F)))
  expect_message(expect_equal(continuousTester$hazard(1, T), log(dexp(1) / pexp(1, lower.tail = F))))
  expect_silent(expect_equal(continuousTester$cumHazard(2), -pexp(2, log = T, lower.tail = F)))
  expect_silent(expect_equal(continuousTester$cumHazard(2, T), log(-pexp(2, log = T, lower.tail = F))))
})



continuousTester <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  pdf = dexpo, cdf = cexpo,
  parameters = ps,
  decorators = ExoticStatistics
)

test_that("anti-derivatives", {
  expect_error(continuousTester$cdfAntiDeriv())
  expect_message(expect_equal(continuousTester$cdfAntiDeriv(lower = 2, upper = 5), 3 + exp(-5) - exp(-2)))
  expect_message(expect_equal(continuousTester$survivalAntiDeriv(), 1))
  expect_message(expect_equal(continuousTester$survivalAntiDeriv(lower = 2, upper = 5), exp(-2) - exp(-5)))
})

test_that("analytic survival functions", {
  expect_silent(expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F)))
  expect_silent(expect_equal(continuousTester$hazard(1), dexp(1) / pexp(1, lower.tail = F)))
  expect_silent(expect_equal(continuousTester$hazard(1, T), log(dexp(1) / pexp(1, lower.tail = F))))
  expect_silent(expect_equal(continuousTester$cumHazard(2), -pexp(2, log = T, lower.tail = F)))
})

test_that("p-norms", {
  expect_error(continuousTester$cdfPNorm(2))
  expect_message(expect_equal(continuousTester$cdfPNorm(p = 2, lower = 2, upper = 5)^2, (5 + 2 * exp(-5) - 0.5 * exp(-10)) - (2 + 2 * exp(-2) - 0.5 * exp(-4))))

  expect_message(expect_equal((continuousTester$pdfPNorm(2))^2, 0.5))
  expect_message(expect_equal(continuousTester$pdfPNorm(p = 2, lower = 2, upper = 5)^2, 0.5 * (exp(-4) - exp(-10))))

  expect_message(expect_equal(continuousTester$squared2Norm(), continuousTester$pdfPNorm(2)))
  expect_message(expect_equal(continuousTester$squared2Norm(lower = 2, upper = 5), continuousTester$pdfPNorm(2, 2, 5)))

  expect_message(expect_equal((continuousTester$survivalPNorm(2))^2, 0.5))
  expect_message(expect_equal(continuousTester$survivalPNorm(p = 2, lower = 2, upper = 5)^2, 0.5 * (exp(-4) - exp(-10))))
})
