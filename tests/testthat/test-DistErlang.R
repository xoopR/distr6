library(testthat)

context("Erlang distribution")

test_that("autottest", {
  autotest_sdistribution(Erlang)
})

test_that("parameterisation", {
  expect_silent(Erlang$new())
  expect_silent(Erlang$new(shape = 2, rate = 3))
  expect_equal(Erlang$new(shape = 2, rate = 3)$getParameterValue("rate"), 3)
  expect_equal(Erlang$new(rate = 3)$getParameterValue("rate"), 3)
  expect_silent(Erlang$new(rate = 3))
  expect_silent(Erlang$new(shape = 2))
  expect_silent(Erlang$new(scale = 2))
  expect_equal(Erlang$new(shape = 2)$getParameterValue("shape"), 2)
  expect_equal(Erlang$new(shape = 2, rate = 3)$getParameterValue("scale"), 1 / 3)
  expect_message(expect_equal(Erlang$new(shape = 2, scale = 3, verbose = T)$getParameterValue("rate"), 1 / 3))
})

test_that("properties & traits", {
  expect_equal(Erlang$new()$valueSupport, "continuous")
  expect_equal(Erlang$new()$variateForm, "univariate")
  expect_equal(Erlang$new()$symmetry, "asymmetric")
  expect_equal(Erlang$new()$sup, Inf)
  expect_equal(Erlang$new()$inf, 0)
  expect_equal(Erlang$new()$dmax, Inf)
  expect_equal(Erlang$new()$dmin, 0)
})


G <- Erlang$new(shape = 1)
test_that("statistics", {
  expect_equal(G$mean(), 1)
  expect_equal(G$variance(), 1)
  expect_equal(G$skewness(), 2)
  expect_equal(G$kurtosis(T), 6)
  expect_equal(G$kurtosis(F), 9)
  expect_equal(G$pgf(1), NaN)
  expect_equal(G$entropy(), 1)
  expect_equal(G$mgf(0), 1)
  expect_equal(G$mgf(5), NaN)
  expect_equal(G$cf(1), 1 / (1 - 1i))
  expect_equal(G$mode(), 0)
  expect_equal(G$pdf(1), dgamma(x = 1, shape = 1, rate = 1))
  expect_equal(G$cdf(1), pgamma(1, shape = 1, rate = 1))
  expect_equal(G$quantile(0.324), qgamma(0.324, shape = 1, rate = 1))
  expect_silent(G$rand(10))
})
