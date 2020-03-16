library(testthat)

tri = TriangularKernel$new()

test_that("zero statistics",{
  expect_equal(tri$mean(), 0)
  expect_equal(tri$median(), 0)
  expect_equal(tri$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(tri$variance(), 1/6)
  expect_equal(tri$squared2Norm(), 2/3)
})


test_that("support",{
  expect_equal(tri$support$strprint(), Interval$new(-1,1)$strprint())
})

test_that("d/p/q/r",{
  expect_equal(tri$pdf(c(-0.1,0,0.1)), c(0.9, 1, 0.9))
  expect_equal(tri$cdf(-0.4), 0.18)
  expect_equal(tri$cdf(tri$quantile(c(0.42,0.5,0.78))),c(0.42,0.5,0.78))
  expect_equal(length(tri$rand(10)), 10)
})
