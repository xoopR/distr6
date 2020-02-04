library(testthat)

triw = Triweight$new()

test_that("zero statistics",{
  expect_equal(triw$mean(), 0)
  expect_equal(triw$median(), 0)
  expect_equal(triw$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(triw$variance(), 1/9)
  expect_equal(triw$squared2Norm(), 350/429)
})

test_that("support",{
  expect_equal(triw$support()$strprint(), Interval$new(-1,1)$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(triw$pdf(c(-0.1,0,0.1)),4), c(1.0613, 1.0938, 1.0613))
  expect_equal(triw$cdf(0.4), 0.873964)
  expect_null(triw$quantile(0.42))
  expect_null(triw$rand(10))
})
