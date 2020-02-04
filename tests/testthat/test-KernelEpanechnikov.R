library(testthat)

epan = Epanechnikov$new()

test_that("zero statistics",{
  expect_equal(epan$mean(), 0)
  expect_equal(epan$median(), 0)
  expect_equal(epan$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(epan$variance(), 1/5)
  expect_equal(epan$squared2Norm(), 3/5)
})

test_that("support",{
  expect_equal(epan$support()$strprint(), Interval$new(-1,1)$strprint())
})

test_that("d/p/q/r",{
  expect_equal(epan$pdf(c(-0.1,0,0.1)), c(0.7425, 0.75, 0.7425))
  expect_equal(epan$cdf(0.4), 0.784)
  expect_null(epan$quantile(0.42))
  expect_null(epan$rand(10))
})
