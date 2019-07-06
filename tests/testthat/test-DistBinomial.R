library(testthat)

context("Binomial distribution")

test_that("constructor", {
  expect_silent(Binomial$new())
  expect_silent(Binomial$new(prob = 0.2))
  expect_silent(Binomial$new(qprob = 0.2))
  expect_equal(Binomial$new(prob = 0.2)$getParameterValue("qprob"), 0.8)
  expect_equal(Binomial$new(qprob = 0.2)$getParameterValue("prob"), 0.8)
})

test_that("properties & traits",{
  expect_equal(Binomial$new()$valueSupport(), "discrete")
  expect_equal(Binomial$new()$variateForm(), "univariate")
  expect_equal(Binomial$new()$symmetry(), "symmetric")
  expect_equal(Binomial$new(prob = 0.1)$symmetry(), "asymmetric")
  expect_equal(Binomial$new(size=12)$sup(), 12)
  expect_equal(Binomial$new()$inf(), 0)
  expect_equal(Binomial$new(size=12)$dmax(), 12)
  expect_equal(Binomial$new()$dmin(), 0)
})

b = Binomial$new()
test_that("statistics",{
  expect_equal(b$mean(), 5)
  expect_equal(b$variance(), 2.5)
  expect_equal(b$skewness(), 0)
  expect_equal(b$kurtosis(T), -0.2)
  expect_equal(b$kurtosis(F), 2.8)
  expect_equal(round(b$entropy(), 5), 2.70806)
  expect_equal(b$mgf(1), (0.5+0.5*exp(1))^10)
  expect_equal(b$cf(1), (0.5+0.5*exp(1i))^10)
  expect_equal(b$pgf(1), 1)
  expect_equal(b$mode(), 5)
  expect_equal(b$pdf(1), dbinom(1,size=10,0.5))
  expect_equal(b$cdf(1), pbinom(1,size=10,0.5))
  expect_equal(b$quantile(0.56), qbinom(0.56,size=10,0.5))
  expect_silent(b$rand(10))
})

