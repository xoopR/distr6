library(testthat)

context("Dirichlet distribution")

test_that("constructor",{
  expect_error(Dirichlet$new())
  expect_error(Dirichlet$new(params = c(0,1,2)))
  expect_silent(Dirichlet$new(params = c(3,1,2)))
})

params = c(0.2,0.5,0.3)
mn = Dirichlet$new(params)
test_that("parameters", {
  expect_equal(mn$getParameterValue("K"), 3)
  expect_equal(mn$getParameterValue("params"), c(0.2,0.5,0.3))
  expect_error(mn$setParameterValue(list(params = 0.2)))
  expect_error(mn$setParameterValue(list(params = c(0.2,0.5,0.2,0.1))))
  expect_silent(mn$setParameterValue(list(params = c(0.2,0.5,0.2))))
})

test_that("properties & traits",{
  expect_equal(mn$valueSupport(), "continuous")
  expect_equal(mn$variateForm(), "multivariate")
  expect_equal(mn$symmetry(), "asymmetric")
  expect_equal(mn$inf(), 0)
  expect_equal(mn$sup(), 1)
  expect_equal(mn$dmin(), 0)
  expect_equal(mn$dmax(), 1)
})

params = c(2,3)
mn = Dirichlet$new(params)
test_that("statistics",{
  expect_equal(mn$mean(), c(2/5,3/5))
  expect_equal(mn$var(), matrix(c(0.04,-0.04,-0.04,0.04),nrow = 2))
  expect_error(mn$skewness())
  expect_error(mn$kurtosis())
  expect_equal(round(mn$entropy(), 5), round(log(1/12,base=2) + 3*digamma(5) - 2.268353,5))
  expect_error(mn$mgf(1))
  expect_error(mn$pgf(1))
  expect_error(mn$cf(1))
  expect_equal(mn$mode(),c(1/3,2/3))
  expect_equal(mn$pdf(0.2,0.8), extraDistr::ddirichlet(c(0.2,0.8),params))
  expect_equal(mn$pdf(c(0.2,0.5),c(0.8,0.5)),
               c(extraDistr::ddirichlet(c(0.2,0.8),params),
                 extraDistr::ddirichlet(c(0.5,0.5),params)))
  expect_null(mn$cdf(1,2))
  expect_null(mn$quantile(0.1,0.2))
  expect_equal(dim(mn$rand(10)),c(10,2))
})
