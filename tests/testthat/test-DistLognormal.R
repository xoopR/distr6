library(testthat)

context("Lognormal distribution")

test_that("constructor",{
  expect_silent(Lognormal$new())
  expect_silent(Lognormal$new(var = 1))
  expect_silent(Lognormal$new(sd = 1))
  expect_silent(Lognormal$new(prec = 1))
  expect_silent(Lognormal$new(var = 2, sd = 3))
  expect_silent(Lognormal$new(var = 2, prec = 3))
  expect_silent(Lognormal$new(prec = 2, sd = 3))
  expect_silent(Lognormal$new(var = 2, sd = 3, prec = 4))
  expect_silent(Lognormal$new(sdlog = 2, prec = 4, mean = 5))
})

test_that("parameterisation",{
  expect_equal(Lognormal$new(var = 2)$getParameterValue("var"), 2)
  expect_equal(Lognormal$new(preclog = 2)$getParameterValue("preclog"), 2)
  expect_equal(Lognormal$new(sd = 2)$getParameterValue("sd"), 2)
  expect_equal(Lognormal$new(meanlog = 2, mean = 2)$getParameterValue("meanlog"),2)

  expect_equal(Lognormal$new(var = 2)$getParameterValue("prec"), 2^-1)
  expect_equal(Lognormal$new(var = 2)$getParameterValue("var"), 2)

  expect_false(Lognormal$new(sd = 2)$parameters("meanlog")$settable)
  expect_true(Lognormal$new(sdlog = 2)$parameters("meanlog")$settable)
})

test_that("properties & traits",{
  expect_equal(Lognormal$new()$symmetry(), "asymmetric")
  expect_equal(Lognormal$new()$inf(), 0)
  expect_equal(Lognormal$new()$sup(), Inf)
  expect_equal(Lognormal$new()$dmin(), .Machine$double.eps)
  expect_equal(Lognormal$new()$dmax(), Inf)
  expect_equal(Lognormal$new()$valueSupport(), "continuous")
  expect_equal(Lognormal$new()$variateForm(), "univariate")
})

ln = Lognormal$new()
test_that("statistics",{
  expect_equal(ln$mean(), exp(1/2))
  expect_equal(ln$var(), (exp(1)-1)*exp(1))
  expect_equal(ln$skewness(), (exp(1)+2)*sqrt(exp(1)-1))
  expect_equal(ln$kurtosis(T), exp(4)+2*exp(3)+3*exp(2)-6)
  expect_equal(ln$kurtosis(F), exp(4)+2*exp(3)+3*exp(2)-3)
  expect_equal(ln$entropy(), log(exp(0.5)*sqrt(2*pi), 2))
  expect_equal(ln$mgf(1), NaN)
  expect_error(ln$cf(1))
  expect_equal(ln$mode(), exp(-1))
  expect_equal(ln$pdf(2:6), dlnorm(2:6))
  expect_equal(ln$cdf(2, log.p = T, lower.tail = F), plnorm(2, log.p = T, lower.tail = F))
  expect_equal(ln$quantile(0.46), qlnorm(0.46))
  expect_equal(length(ln$rand(10)),10)
})
