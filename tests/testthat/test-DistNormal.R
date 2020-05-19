library(testthat)

context("Normal distribution")

test_that("autottest", {
  autotest_sdistribution(Normal)
})

test_that("constructor", {
  expect_silent(Normal$new())
  expect_silent(Normal$new(var = 1))
  expect_silent(Normal$new(sd = 1))
  expect_silent(Normal$new(prec = 1))
  expect_message(Normal$new(var = 2, sd = 3, verbose = T))
  expect_silent(Normal$new(var = 2, prec = 3))
  expect_message(Normal$new(prec = 2, sd = 3, verbose = T))
  expect_silent(Normal$new(var = 2, sd = 3, prec = 4))

  expect_equal(Normal$new(var = 2)$getParameterValue("sd"), 2^0.5)
  expect_equal(Normal$new(var = 2)$getParameterValue("prec"), 2^-1)
  expect_equal(Normal$new(var = 2)$getParameterValue("var"), 2)

  expect_equal(Normal$new(sd = 2)$getParameterValue("sd"), 2)
  expect_equal(Normal$new(sd = 2)$getParameterValue("prec"), 2^-2)
  expect_equal(Normal$new(sd = 2)$getParameterValue("var"), 2^2)

  expect_equal(Normal$new(prec = 2)$getParameterValue("sd"), 2^-0.5)
  expect_equal(Normal$new(prec = 2)$getParameterValue("prec"), 2)
  expect_equal(Normal$new(prec = 2)$getParameterValue("var"), 2^-1)

  expect_equal(Normal$new(var = 2, sd = 3)$getParameterValue("sd"), 3)
  expect_equal(Normal$new(var = 2, prec = 3)$getParameterValue("var"), 1 / 3)
  expect_equal(Normal$new()$getParameterValue("var"), 1)
  expect_equal(Normal$new(sd = 1, prec = 2)$getParameterValue("sd"), sqrt(1 / 2))
  expect_equal(Normal$new(sd = 3, prec = 2)$getParameterValue("prec"), 2)
})

test_that("properties & traits", {
  expect_equal(Normal$new()$symmetry, "symmetric")
  expect_equal(Normal$new()$inf, -Inf)
  expect_equal(Normal$new()$sup, Inf)
  expect_equal(Normal$new()$dmin, -Inf)
  expect_equal(Normal$new()$dmax, Inf)
  expect_equal(Normal$new()$valueSupport, "continuous")
  expect_equal(Normal$new()$variateForm, "univariate")
})

test_that("statistics", {
  expect_equal(Normal$new()$mean(), 0)
  expect_equal(Normal$new()$variance(), 1)
  expect_equal(Normal$new()$skewness(), 0)
  expect_equal(Normal$new()$kurtosis(T), 0)
  expect_equal(Normal$new()$kurtosis(F), 3)
  expect_equal(Normal$new()$entropy(), 0.5 * log(2 * pi * exp(1), base = 2))
  expect_equal(Normal$new()$mgf(1), exp(0.5))
  expect_equal(Normal$new()$pgf(1), NaN)
  expect_equal(Normal$new()$cf(1), as.complex(exp(-0.5)))
  expect_equal(Normal$new()$mode(), 0)
  expect_equal(Normal$new()$pdf(2), dnorm(2))
  expect_equal(Normal$new()$cdf(2), pnorm(2))
  expect_equal(Normal$new()$quantile(0.46), qnorm(0.46))
  expect_equal(Normal$new()$quantile(-log(5), log.p = T, lower.tail = F), qnorm(-log(5), log.p = T, lower.tail = F))
  expect_equal(nrow(Normal$new()$quantile(0.23, simplify = F)), 1)
  expect_equal(length(Normal$new()$rand(10)), 10)
  expect_equal(nrow(Normal$new()$rand(10, simplify = F)), 10)
  expect_equal(nrow(Normal$new()$pdf(10, simplify = F)), 1)
  expect_equal(nrow(Normal$new()$cdf(10, simplify = F)), 1)
  expect_equal(nrow(Normal$new()$quantile(10, simplify = F)), 1)
})
