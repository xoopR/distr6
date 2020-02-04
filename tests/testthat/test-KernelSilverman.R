library(testthat)

silv = Silverman$new()

test_that("zero statistics",{
  expect_equal(silv$mean(), 0)
  expect_equal(silv$median(), 0)
  expect_equal(silv$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(silv$variance(), 0)
  expect_equal(silv$squared2Norm(), (3*sqrt(2))/16)
})

test_that("support",{
  expect_equal(silv$support$strprint(), Reals$new()$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(silv$pdf(c(-0.1,0,0.1)),4), c(0.3519, 0.3536,  0.3519))
  expect_null(silv$cdf(0.42))
  expect_null(silv$quantile(0.42))
  expect_null(silv$rand(10))
})
