library(testthat)

sigm = Sigmoid$new()

test_that("zero statistics",{
  expect_equal(sigm$mean(), 0)
  expect_equal(sigm$median(), 0)
  expect_equal(sigm$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(sigm$variance(), (pi^2)/4)
  expect_equal(sigm$squared2Norm(), 2/(pi^2))
})

test_that("support",{
  expect_equal(sigm$support$strprint(), Reals$new()$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(sigm$pdf(c(-0.1,0,0.1)),4), c(0.3167, 0.3183,  0.3167))
  expect_null(sigm$cdf(0.42))
  expect_null(sigm$quantile(0.42))
  expect_null(sigm$rand(10))
})
