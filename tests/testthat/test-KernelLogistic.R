library(testthat)

logis = LogisticKernel$new()

test_that("zero statistics",{
  expect_equal(logis$mean(), 0)
  expect_equal(logis$median(), 0)
  expect_equal(logis$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(logis$variance(), (pi^2)/3)
  expect_equal(logis$squared2Norm(), 1/6)
})

test_that("support",{
  expect_equal(logis$support()$getSymbol(), Reals$new()$getSymbol())
})

test_that("d/p/q/r",{
  expect_equal(round(logis$pdf(c(-0.1,0,0.1)),4), c(0.2494, 0.2500, 0.2494))
  expect_equal(round(logis$cdf(0.4),4), 0.5987)
  expect_equal(logis$cdf(logis$quantile(c(0.42,0.5,0.78))),c(0.42,0.5,0.78))
  expect_equal(length(logis$rand(10)), 10)
})

