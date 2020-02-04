library(testthat)

context("DiscreteUniform distribution")

test_that("constructor",{
  expect_silent(DiscreteUniform$new())
  expect_error(DiscreteUniform$new(lower = 5, upper = 3))
  expect_silent(DiscreteUniform$new(lower = 2, upper = 8))
})

test_that("parameters", {
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("lower"),2)
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("upper"),3)
  expect_equal(DiscreteUniform$new(lower = 2, upper = 3)$getParameterValue("N"),2)
})

test_that("properties & traits",{
  expect_equal(DiscreteUniform$new()$valueSupport, "discrete")
  expect_equal(DiscreteUniform$new()$variateForm, "univariate")
  expect_equal(DiscreteUniform$new()$symmetry, "symmetric")
  expect_equal(DiscreteUniform$new()$sup, 1)
  expect_equal(DiscreteUniform$new()$inf, 0)
  expect_equal(DiscreteUniform$new()$dmax, 1)
  expect_equal(DiscreteUniform$new()$dmin, 0)
})

du = DiscreteUniform$new()
test_that("statistics",{
  expect_equal(du$mean(), 0.5)
  expect_equal(du$variance(), 0.25)
  expect_equal(du$skewness(), 0)
  expect_equal(du$kurtosis(T), -2)
  expect_equal(du$kurtosis(F), 1)
  expect_equal(du$entropy(), log(2,2))
  expect_equal(du$mgf(1),  (1-exp(2))/(2*(1-exp(1))))
  expect_equal(du$cf(1), (1-exp(2i))/(2*(1-exp(1i))))
  expect_equal(du$pgf(1), 1)
  expect_equal(du$mode(),du$inf:du$sup)
  expect_equal(du$mode(2),1)
  expect_equal(du$pdf(1), 1/2)
  expect_equal(du$cdf(1), 1)
  expect_equal(du$quantile(0.324), 0)
  expect_silent(du$rand(10))
})

test_that("update parameters",{
  du = DiscreteUniform$new(lower=1,upper=5)
  expect_error(du$setParameterValue(lst = list(upper = 0)))
  expect_silent(du$setParameterValue(lst = list(upper = 2)))
  expect_error(du$setParameterValue(lst = list(lower = 3)))
  expect_silent(du$setParameterValue(lst = list(lower = 1)))
  expect_silent(du$setParameterValue(lst = list(lower = 1, upper = 3)))
  expect_error(du$setParameterValue(lst = list(lower = 1, upper = 0)))
})
