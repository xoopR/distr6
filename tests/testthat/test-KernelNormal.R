library(testthat)

norm = NormalKernel$new()

test_that("zero statistics",{
  expect_equal(norm$mean(), 0)
  expect_equal(norm$median(), 0)
  expect_equal(norm$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(norm$variance(), 1)
  expect_equal(norm$squared2Norm(), 1/(2*sqrt(pi)))
})

test_that("support",{
  expect_equal(norm$support$strprint(), Reals$new()$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(norm$pdf(c(-0.1,0,0.1)),4), c(0.3970, 0.3989, 0.3970))
  expect_equal(round(norm$cdf(0.4),4), 0.6554)
  expect_equal(norm$cdf(norm$quantile(c(0.42,0.5,0.78))),c(0.42,0.5,0.78))
  expect_equal(length(norm$rand(10)), 10)
})
