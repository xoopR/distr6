library(testthat)

context("Logistic distribution")

test_that("constructor",{
  expect_silent(Logistic$new())
  expect_silent(Logistic$new(mean = 1))
  expect_silent(Logistic$new(scale = 1))
  expect_silent(Logistic$new(sd = 2))
  expect_error(Logistic$new(scale = 0))

  expect_equal(Logistic$new(mean = 2)$getParameterValue("mean"), 2)
  expect_equal(Logistic$new(scale = 2)$getParameterValue("scale"), 2)
})

l = Logistic$new()
test_that("properties & traits",{
  expect_equal(l$symmetry(), "symmetric")
  expect_equal(l$inf(), -Inf)
  expect_equal(l$sup(), Inf)
  expect_equal(l$dmin(), -Inf)
  expect_equal(l$dmax(), Inf)
  expect_equal(l$valueSupport(), "continuous")
  expect_equal(l$variateForm(), "univariate")
})

test_that("statistics",{
  expect_equal(l$mean(), 0)
  expect_equal(l$mode(), 0)
  expect_equal(l$var(), pi^2/3)
  expect_equal(l$skewness(), 0)
  expect_equal(l$kurtosis(T), 1.2)
  expect_equal(l$kurtosis(F), 4.2)
  expect_equal(l$entropy(), 2)
  expect_equal(l$mgf(1), NaN)
  expect_equal(l$mgf(0.4), beta(0.6,1.4))
  expect_equal(l$cf(1), as.complex(pi / sinh(pi)))
  expect_equal(l$pdf(2), dlogis(2))
  expect_equal(l$cdf(2), plogis(2))
  expect_equal(l$quantile(0.4234), qlogis(0.4234))
  expect_equal(l$cdf(l$quantile(0.4234)), 0.4234)
  expect_silent(l$rand(10))
})
