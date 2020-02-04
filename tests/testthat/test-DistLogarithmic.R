library(testthat)

context("Logarithmic distribution")

test_that("properties & traits",{
  expect_equal(Logarithmic$new()$valueSupport, "discrete")
  expect_equal(Logarithmic$new()$variateForm, "univariate")
  expect_equal(Logarithmic$new()$symmetry, "asymmetric")
  expect_equal(Logarithmic$new()$sup, Inf)
  expect_equal(Logarithmic$new()$inf, 1)
  expect_equal(Logarithmic$new()$dmax, Inf)
  expect_equal(Logarithmic$new()$dmin, 1)
})

l = Logarithmic$new()
test_that("statistics",{
  expect_equal(l$mean(), -1/log(0.5))
  expect_equal(l$variance(), (-0.25 - 0.5*log(0.5))/(0.25*log(0.5)^2))
  expect_equal(round(l$skewness(), 4), 3.0148)
  expect_equal(round(l$kurtosis(T), 4), 13.3884)
  expect_equal(round(l$kurtosis(F), 4), 16.3884)
  expect_error(l$entropy())
  expect_equal(l$mgf(1), NaN)
  expect_equal(l$mgf(0.5), log(1-0.5*exp(0.5))/log(0.5))
  expect_equal(l$cf(0.5), log(1-0.5*exp(0.5i))/log(0.5))
  expect_equal(l$pgf(3), NaN)
  expect_equal(l$pgf(0.5), log(0.75)/log(0.5))
  expect_equal(l$mode(), 1)
  expect_equal(l$pdf(1,log=T), extraDistr::dlgser(1,0.5, log = T))
  expect_equal(l$cdf(1), extraDistr::plgser(1,0.5))
  expect_equal(l$quantile(1), extraDistr::qlgser(1,0.5))
  expect_silent(l$rand(10))
})

