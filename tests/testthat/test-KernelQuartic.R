library(testthat)

quart = Quartic$new()

test_that("var & squared-norm",{
  expect_equal(quart$variance(), 1/7)
  expect_equal(quart$squared2Norm(), 5/7)
})

test_that("support",{
  expect_equal(quart$support()$getSymbol(), Interval$new(-1,1)$getSymbol())
})

test_that("d/p/q/r",{
  expect_equal(round(quart$pdf(c(-0.1,0,0.1)),4), c(0.9188, 15/16, 0.9188))
  expect_equal(quart$cdf(0.4), 0.83692)
  expect_null(quart$quantile(0.42))
  expect_null(quart$rand(10))
})

