library(testthat)

context("Degenerate distribution")

test_that("autottest", {
  autotest_sdistribution(Degenerate)
})

test_that("constructor", {
  expect_silent(Degenerate$new())
  expect_silent(Degenerate$new(5))
})

test_that("parameters", {
  expect_equal(Degenerate$new()$getParameterValue("mean"), 0)
  expect_equal(Degenerate$new(10)$getParameterValue("mean"), 10)
})

test_that("properties & traits", {
  expect_equal(Degenerate$new()$valueSupport, "discrete")
  expect_equal(Degenerate$new()$variateForm, "univariate")
  expect_equal(Degenerate$new()$symmetry, "symmetric")
  expect_equal(Degenerate$new()$sup, 0)
  expect_equal(Degenerate$new()$inf, 0)
  expect_equal(Degenerate$new()$dmin, 0)
  expect_equal(Degenerate$new()$dmax, 0)
})

test_that("statistics", {
  expect_equal(Degenerate$new()$kurtosis(), NaN)
  expect_equal(Degenerate$new()$skewness(), NaN)
  expect_equal(Degenerate$new()$entropy(), 0)
  expect_equal(Degenerate$new()$variance(), 0)
  expect_equal(Degenerate$new()$stdev(), 0)
  expect_equal(Degenerate$new()$mean(), 0)
  expect_equal(Degenerate$new()$mode(), 0)
  expect_silent(Degenerate$new()$mgf(1))
  expect_silent(Degenerate$new()$cf(1))
  expect_equal(Degenerate$new()$pdf(1), 0)
  expect_equal(Degenerate$new()$pdf(0), 1)
  expect_equal(Degenerate$new()$cdf(-1), 0)
  expect_equal(Degenerate$new()$cdf(0), 1)
  expect_equal(Degenerate$new()$quantile(0.4), 0)
  expect_equal(Degenerate$new()$quantile(0), 0)
  expect_equal(Degenerate$new()$rand(1), 0)
})
