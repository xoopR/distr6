library(testthat)

tric = Tricube$new()

test_that("zero statistics",{
  expect_equal(tric$mean(), 0)
  expect_equal(tric$median(), 0)
  expect_equal(tric$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(tric$variance(), 35/243)
  expect_equal(tric$squared2Norm(), 175/247)
})

test_that("support",{
  expect_equal(tric$support()$strprint(), Interval$new(-1,1)$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(tric$pdf(c(-0.1,0,0.1)),4), c(0.8616, 0.8642, 0.8616))
  expect_null(tric$cdf(0.42))
  expect_null(tric$quantile(0.42))
  expect_null(tric$rand(10))
})
