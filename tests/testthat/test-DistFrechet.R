library(testthat)

context("Frechet distribution")

test_that("parameterisation",{
  expect_silent(Frechet$new())
  expect_silent(Frechet$new(shape = 2, scale = 3))
  expect_error(Frechet$new(shape = 0))
})

test_that("properties & traits",{
  expect_equal(Frechet$new()$symmetry(), "asymmetric")
  expect_equal(Frechet$new(minimum = 3)$inf(), 3)
  expect_equal(Frechet$new(minimum = 3)$sup(), Inf)
  expect_equal(Frechet$new()$dmax(), Inf)
  expect_equal(Frechet$new(minimum=3)$dmin(), 3 + .Machine$double.eps)
  expect_equal(Frechet$new(scale = 3)$valueSupport(), "continuous")
  expect_equal(Frechet$new(scale = 3)$variateForm(), "univariate")
})

x = Frechet$new()
test_that("statistics",{
  expect_equal(x$mean(), Inf)
  expect_equal(Frechet$new(shape = 2)$mean(), gamma(1- 1/2))

  expect_equal(x$var(), Inf)
  expect_equal(Frechet$new(shape = 3)$var(), gamma(1- 2/3) - gamma(1-1/3)^2)

  expect_equal(x$skewness(), Inf)
  expect_equal(Frechet$new(shape = 4)$skewness(), (gamma(0.25) - 3*gamma(0.5)*gamma(0.75) +
                                                     2*gamma(0.75)^3)/(gamma(0.5)-gamma(0.75)^2)^(3/2))


  expect_equal(x$kurtosis(), Inf)
  expect_equal(Frechet$new(shape = 5)$kurtosis(T), ((gamma(0.2)-4*gamma(0.4)*gamma(0.8)+3*gamma(0.6)^2)/
                 ((gamma(0.6) - gamma(0.8)^2)^2)) - 6)
  expect_equal(Frechet$new(shape = 5)$kurtosis(F), ((gamma(0.2)-4*gamma(0.4)*gamma(0.8)+3*gamma(0.6)^2)/
                                                      ((gamma(0.6) - gamma(0.8)^2)^2)) - 3)

  expect_equal(x$entropy(), 1 - 2*digamma(1))
  expect_error(Frechet$new()$mgf(1))
  expect_error(Frechet$new()$cf(1))
  expect_equal(x$mode(), 1/2)

  expect_equal(x$pdf(1:2), extraDistr::dfrechet(1:2))
  expect_equal(x$cdf(1:2), extraDistr::pfrechet(1:2))
  expect_equal(x$quantile(c(0.33,0.45)), extraDistr::qfrechet(c(0.33,0.45)))
  expect_equal(x$cdf(x$quantile(0.46)), 0.46)
  expect_equal(length(Frechet$new()$rand(1:10)),10)
})
