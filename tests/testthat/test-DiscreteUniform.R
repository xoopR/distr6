library(testthat)

context("DiscreteUniform distribution")

test_that("constructor",{
  expect_silent(DiscreteUniform$new())
  expect_error(DiscreteUniform$new(lower = 5, upper = 3))
  expect_silent(DiscreteUniform$new(lower = 2.3, upper = 3))
  expect_silent(DiscreteUniform$new(lower = 2, upper = 8))
})

test_that("parameters", {
  expect_equal(DiscreteUniform$new(lower = 2.3, upper = 3)$getParameterValue("lower"),2)
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("lower"),2)
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("upper"),3)
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("N"),2)
})

test_that("properties & traits",{
  expect_equal(DiscreteUniform$new()$valueSupport(), "discrete")
  expect_equal(DiscreteUniform$new()$variateForm(), "univariate")
  expect_equal(DiscreteUniform$new()$symmetry(), "asymmetric")
})

test_that("silent statistics",{
  du = DiscreteUniform$new()
  expect_silent(du$kurtosis(T))
  expect_silent(du$kurtosis(F))
  expect_silent(du$skewness())
  expect_silent(du$mean())
  expect_silent(du$entropy())
  expect_silent(du$mgf(1))
  expect_silent(du$cf(1))
  expect_silent(du$pgf(1))
  expect_silent(du$pdf(1))
  expect_silent(du$cdf(1))
  expect_silent(du$quantile(1))
  expect_silent(du$rand(1))
  expect_silent(du$var())
  expect_silent(du$sd())
})

test_that("statistical results",{
  du = DiscreteUniform$new(lower=1,upper=5)
  expect_equal(du$pdf(1:5), rep(1/5, 5))
  expect_equal(du$cdf(2, lower.tail = F, log.p = T), extraDistr::pdunif(2,1,5,F,T))
  expect_equal(du$quantile(0.2352), extraDistr::qdunif(0.2352,1,5))
  expect_equal(du$mean(), 3)
})

test_that("update parameters",{
  du = DiscreteUniform$new(lower=1,upper=5)
  expect_error(du$setParameterValue(list(upper = 0)))
  expect_silent(du$setParameterValue(list(upper = 2)))
  expect_error(du$setParameterValue(list(lower = 3)))
  expect_silent(du$setParameterValue(list(lower = 1)))
  expect_silent(du$setParameterValue(list(lower = 1, upper = 3)))
  expect_error(du$setParameterValue(list(lower = 1, upper = 0)))
})
