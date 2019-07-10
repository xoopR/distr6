library(testthat)

context("Array Distributions")

test_that("constructor",{
  expect_silent(ArrayDistribution$new(Binomial, list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ArrayDistribution$new(Binomial, list(list(prob = 2, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ArrayDistribution$new(Binomial, list(list(re = 2, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ArrayDistribution$new(Dist, list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
})

test_that("pdf/cdf",{
  a = ArrayDistribution$new(Binomial, list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                           list(prob = 0.2, size = 6)))
  expect_equal(a$pdf(1,x2 = 2,x3 = 3), Binomial$new(2,0.1)$pdf(1) * Binomial$new(4,0.6)$pdf(2) * Binomial$new(6,0.2)$pdf(3))
  expect_equal(a$cdf(1,x2 = 2,x3 = 3), Binomial$new(2,0.1)$cdf(1) * Binomial$new(4,0.6)$cdf(2) * Binomial$new(6,0.2)$cdf(3))
})

test_that("type/support",{
  a = ArrayDistribution$new(Binomial, list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                           list(prob = 0.2, size = 6)))
  expect_equal(a$type()$getSymbol(), Naturals$new(dim = 3)$getSymbol())
  expect_equal(a$support()$getSymbol(), (Set$new(0:2) * Set$new(0:4) * Set$new(0:6))$getSymbol())

})
