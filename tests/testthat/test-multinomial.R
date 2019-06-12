library(testthat)

context("Multinomial distribution")

test_that("constructor",{
  expect_error(Multinomial$new())
  expect_error(Multinomial$new(size = 5))
  expect_error(Multinomial$new(probs = c(0.2)))
  expect_silent(Multinomial$new(size = 3, probs = c(0.1,0.4)))
})

test_that("parameters", {
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$getParameterValue("K"), 2)
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$getParameterValue("size"), 3)
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$getParameterValue("probs"), c(0.1,0.9))
})

test_that("properties & traits",{
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$valueSupport(), "discrete")
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$variateForm(), "multivariate")
  expect_equal(Multinomial$new(size = 3, probs = c(0.1,0.9))$symmetry(), "asymmetric")
})

test_that("normalise", {
  expect_equal(Multinomial$new(size = 1, probs = c(1,2))$getParameterValue("probs"), c(1/3,2/3))
  expect_equal(Multinomial$new(size = 1, probs = c(0.1,0.9))$getParameterValue("probs"),c(0.1,0.9))
})

test_that("silent statistics",{
  mn = Multinomial$new(size = 3, prob = c(0.1, 0.2, 0.7))
  expect_silent(mn$kurtosis(T))
  expect_silent(mn$skewness())
  expect_silent(mn$mean())
  expect_silent(mn$entropy())
  expect_silent(mn$mgf(1:3))
  expect_silent(mn$cf(1:3))
  expect_silent(mn$pgf(1:3))
  expect_silent(mn$pdf(1:3))
  expect_silent(mn$var())
  expect_silent(mn$sd())
})

test_that("statistical results",{
  probs = c(0.1, 0.2, 0.7)
  mn = Multinomial$new(size = 3, prob = probs)
  expect_equal(mn$pdf(c(1,5,7)), 0)
  expect_equal(mn$pdf(c(1,1,1)), dmultinom(x = c(1,1,1), prob = probs))
  expect_equal(mn$mean(), 3 * probs)
  expect_equal(mn$var(), 3 * probs * (1-probs))
})
