library(testthat)

unif = UniformKernel$new()

test_that("zero statistics",{
  expect_equal(unif$mean(), 0)
  expect_equal(unif$median(), 0)
  expect_equal(unif$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(unif$variance(), 1/3)
  expect_equal(unif$squared2Norm(), 1/2)
})

test_that("support",{
  expect_equal(unif$support()$getSymbol(), Interval$new(-1,1)$getSymbol())
})

test_that("d/p/q/r",{
  expect_equal(unif$pdf(c(-0.1,0,0.1)), rep(0.5, 3))
  expect_equal(unif$cdf(c(0.4)), 0.7)
  expect_equal(unif$cdf(unif$quantile(c(0.42,0.5,0.78))),c(0.42,0.5,0.78))
  expect_equal(length(unif$rand(10)), 10)
})
