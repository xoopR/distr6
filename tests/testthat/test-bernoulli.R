library(testthat)

context("Bernoulli distribution")

test_that("properties & traits",{
  expect_equal(Bernoulli$new()$valueSupport(), "discrete")
  expect_equal(Bernoulli$new()$variateForm(), "univariate")
  expect_equal(Bernoulli$new()$symmetry(), "asymmetric")
})

test_that("silent statistics",{
  expect_silent(Bernoulli$new()$kurtosis(T))
  expect_silent(Bernoulli$new()$kurtosis(F))
  expect_silent(Bernoulli$new()$mean())
  expect_silent(Bernoulli$new()$entropy())
  expect_silent(Bernoulli$new()$mgf(1))
  expect_silent(Bernoulli$new()$cf(1))
  expect_silent(Bernoulli$new()$pgf(1))
  expect_silent(Bernoulli$new()$pdf(1))
  expect_silent(Bernoulli$new()$cdf(1))
  expect_silent(Bernoulli$new()$quantile(1))
  expect_silent(Bernoulli$new()$rand(1))
})

test_that("statistical results",{
  expect_equal(Bernoulli$new()$pdf(1), dbinom(1,1,0.5))
  expect_equal(Bernoulli$new()$cdf(1), pbinom(1,0,0.5))
  expect_equal(Bernoulli$new()$quantile(0.56), qbinom(0.56,1,0.5))
  expect_equal(Bernoulli$new(0.2)$mean(), 0.2)
  expect_equal(Bernoulli$new(0.2)$var(), 0.2*0.8)
})
