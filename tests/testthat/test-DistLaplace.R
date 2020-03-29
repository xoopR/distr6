library(testthat)

context("Laplace distribution")

test_that("autottest",{
  autotest_sdistribution(Laplace)
})

test_that("constructor",{
  expect_silent(Laplace$new())
  expect_silent(Laplace$new(var = 1))
  expect_silent(Laplace$new(scale = 1))
  expect_silent(Laplace$new(mean = 3))
  expect_warning(Laplace$new(var = -2))

  expect_message(expect_equal(Laplace$new(var = 2, verbose = T)$getParameterValue("var"), 2))
  expect_equal(Laplace$new(var = 2)$getParameterValue("scale"), 1)
  expect_equal(Laplace$new(scale = 2)$getParameterValue("var"), 8)
  expect_equal(Laplace$new(scale = 2)$getParameterValue("scale"), 2)
})

test_that("properties & traits",{
  expect_equal(Laplace$new()$symmetry, "symmetric")
  expect_equal(Laplace$new()$inf, -Inf)
  expect_equal(Laplace$new()$sup, Inf)
  expect_equal(Laplace$new()$dmin, -Inf)
  expect_equal(Laplace$new()$dmax, Inf)
  expect_equal(Laplace$new()$valueSupport, "continuous")
  expect_equal(Laplace$new()$variateForm, "univariate")
})

test_that("statistics",{
  expect_equal(Laplace$new()$mean(), 0)
  expect_equal(Laplace$new()$variance(), 2)
  expect_equal(Laplace$new()$skewness(), 0)
  expect_equal(Laplace$new()$kurtosis(T), 3)
  expect_equal(Laplace$new()$kurtosis(F), 6)
  expect_equal(Laplace$new()$pgf(1), NaN)
  expect_equal(Laplace$new()$entropy(), log(2*exp(1),2))
  expect_equal(Laplace$new()$mgf(2), NaN)
  expect_equal(Laplace$new()$mgf(0.5), (1-0.5^2)^-1)
  expect_equal(Laplace$new()$cf(1), as.complex(0.5))
  expect_equal(Laplace$new()$mode(), 0)
  expect_equal(Laplace$new()$pdf(2), 0.5*exp(-2))
  expect_equal(Laplace$new()$cdf(2), 1-0.5*exp(-2))
  expect_equal(Laplace$new()$quantile(0.46), log(2*0.46))
  expect_silent(Laplace$new()$rand(10))
})
