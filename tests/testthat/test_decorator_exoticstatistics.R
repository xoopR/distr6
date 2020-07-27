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

ps <- getParameterSet.Exponential()

continuousTester <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  pdf = dexpo,
  cdf = cexpo,
  parameters = ps,
  decorators = c("CoreStatistics", "ExoticStatistics")
)

test_that("numeric survival functions", {
  expect_equal(continuousTester$survival(1), pexp(1, lower.tail = F))
  expect_equal(continuousTester$hazard(1), dexp(1) / pexp(1, lower.tail = F))
  expect_equal(continuousTester$hazard(1, log = T), log(dexp(1) / pexp(1, lower.tail = F)))
  expect_equal(continuousTester$cumHazard(2), -pexp(2, log.p = T, lower.tail = F))
  expect_equal(continuousTester$cumHazard(2, log = T), log(-pexp(2, log.p = T, lower.tail = F)))
})
#
# continuousTester <- Distribution$new("Continuous Test", "ContTest",
#   support = PosReals$new(),
#   symmetric = TRUE, type = PosReals$new(zero = T),
#   cdf = cexpo,
#   parameters = ps,
#   decorators = "ExoticStatistics"
# )
#
# test_that("numeric survival functions", {
#   expect_silent(expect_equal(continuousTester$survival(1),
#    pexp(1, lower.tail = F)))
#   expect_message(expect_equal(continuousTester$hazard(1),
#    dexp(1) / pexp(1, lower.tail = F)))
#   expect_message(expect_equal(continuousTester$hazard(1, T),
#   log(dexp(1) / pexp(1, lower.tail = F))))
#   expect_silent(expect_equal(continuousTester$cumHazard(2),
#   -pexp(2, log = T, lower.tail = F)))
#   expect_silent(expect_equal(continuousTester$cumHazard(2, T), l
#   og(-pexp(2, log.p = T, lower.tail = F))))
# })

dist <- Binomial$new(decorators = "ExoticStatistics")

test_that("anti-derivatives", {
  expect_error(continuousTester$cdfAntiDeriv())
  expect_message(expect_equal(continuousTester$cdfAntiDeriv(lower = 2, upper = 5),
                              3 + exp(-5) - exp(-2)))
  expect_message(expect_equal(continuousTester$survivalAntiDeriv(), 1))
  expect_message(expect_equal(continuousTester$survivalAntiDeriv(lower = 2, upper = 5),
                              exp(-2) - exp(-5)))

  expect_equal(dist$survivalAntiDeriv(), dist$mean())
  expect_rounded_equal(dist$survivalAntiDeriv(2, 5), 2.40, 2)
  expect_rounded_equal(dist$cdfAntiDeriv(lower = 2, upper = 5), 0.604, 3)
  expect_rounded_equal(dist$cdfAntiDeriv(), 5)
})

test_that("p-norms", {
  expect_error(continuousTester$cdfPNorm(2))
  expect_message(expect_equal(continuousTester$cdfPNorm(p = 2, lower = 2, upper = 5)^2,
                              (5 + 2 * exp(-5) - 0.5 * exp(-10)) - (2 + 2 * exp(-2) - 0.5 *
                                                                      exp(-4))))

  expect_message(expect_equal((continuousTester$pdfPNorm(2))^2, 0.5))
  expect_message(expect_equal(continuousTester$pdfPNorm(p = 2, lower = 2, upper = 5)^2, 0.5 *
                                (exp(-4) - exp(-10))))

  expect_message(expect_rounded_equal(continuousTester$survivalPNorm(2), 0.71, 2))
  expect_message(expect_rounded_equal(continuousTester$survivalPNorm(2, 2, 5), 0.1, 2))

  expect_rounded_equal(dist$survivalPNorm(2, lower = 2, upper = 4), 1.58, 2)
  expect_rounded_equal(dist$cdfPNorm(2, lower = 2, upper = 4), 0.03, 2)
  expect_rounded_equal(dist$pdfPNorm(2, lower = 2, upper = 4), 0.02, 2)
  expect_rounded_equal(dist$survivalPNorm(2), 4.12, 2)
  expect_rounded_equal(dist$cdfPNorm(2), 4.12, 2)
  expect_rounded_equal(dist$pdfPNorm(2), 0.18, 2)
})
