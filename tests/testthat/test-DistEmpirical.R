library(testthat)

context("Empirical distribution")

test_that("samples constructor",{
  expect_silent(Empirical$new(1:100))
  expect_error(Empirical$new(1,2,3))
  expect_error(Empirical$new())
  expect_null(Empirical$new(1:10)$getParameterValue(1))
  expect_message(expect_null(Empirical$new(1:10)$setParameterValue(1)))
})

emp = Empirical$new(1:10)
test_that("properties & traits",{
  expect_equal(emp$valueSupport(), "discrete")
  expect_equal(emp$variateForm(), "univariate")
  expect_equal(emp$symmetry(), "asymmetric")
})


test_that("statistics",{
  expect_equal(emp$mean(), mean(1:10))
  expect_equal(emp$variance(), var(1:10)*9/10)
  expect_error(emp$skewness())
  expect_error(emp$kurtosis())
  expect_error(emp$entropy())
  expect_error(emp$mgf(1:3))
  expect_error(emp$pgf(1:3))
  expect_error(emp$cf(1:3))
  expect_equal(emp$mode(), 1:10)
  expect_equal(emp$mode(which = 2), 2)
  expect_equal(emp$pdf(2), 1/10)
  expect_equal(Empirical$new(c(1,2,3,2,2))$pdf(1:2), c(1/5, 3/5))
  expect_equal(Empirical$new(c(1,2,3,2,2))$cdf(c(1,2.5)), c(1/5, 4/5))
  set.seed(42)
  emp = Empirical$new(runif(1000)*100)
  expect_equal(round(emp$quantile(emp$cdf(c(2,5,6,4,1,30,20,2,2,2)))),c(2,5,6,4,1,30,20,2,2,2))
  expect_equal(length(emp$rand(10)),10)
})
