library(testthat)

cos = Cosine$new()

test_that("zero statistics",{
  expect_equal(cos$mean(), 0)
  expect_equal(cos$median(), 0)
  expect_equal(cos$mode(), 0)
})

test_that("var & squared-norm",{
  expect_equal(cos$variance(), 1-(8/(pi^2)))
  expect_equal(cos$squared2Norm(), (pi^2)/16)
})

test_that("support",{
  expect_equal(cos$support()$strprint(), Interval$new(-1,1)$strprint())
})

test_that("d/p/q/r",{
  expect_equal(round(cos$pdf(c(-0.1,0,0.1)),4), c(0.7757, 0.7854, 0.7757))
  expect_equal(round(cos$cdf(0.4),4), 0.7939)
  expect_equal(cos$cdf(cos$quantile(c(0.42,0.5,0.78))),c(0.42,0.5,0.78))
  expect_equal(length(cos$rand(10)), 10)
})
