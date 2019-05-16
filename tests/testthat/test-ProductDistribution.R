library(testthat)

context("Product Distribution")

test_that("constructor",{
  expect_silent(ProductDistribution$new(Binomial$new(),Binomial$new(size = 20, prob = 0.6)))
  expect_silent(ProductDistribution$new(Binomial$new(),Exponential$new(rate=1)))
})

test_that("type/support/distrDomain",{
  expect_equal(ProductDistribution$new(Binomial$new(), Binomial$new())$type()$getSymbol(), PosIntegers$new(2)$getSymbol())
  expect_equal(ProductDistribution$new(Binomial$new(size = 2), Binomial$new(size = 3))$support()$getSymbol(),
               product(Set$new(0:2),Set$new(0:3))$getSymbol())
  expect_equal(ProductDistribution$new(Binomial$new(), Binomial$new())$distrDomain()$getSymbol(), PosIntegers$new(2)$getSymbol())
})

test_that("pdf/cdf",{
  expect_equal(ProductDistribution$new(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9))$pdf(x1=4,x2=8),
               Binomial$new(size = 40, prob = 0.2)$pdf(4) * Binomial$new(size = 5, prob = 0.9)$pdf(8))
  expect_equal(ProductDistribution$new(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9))$pdf(x1=4,x2=3),
               Binomial$new(size = 40, prob = 0.2)$pdf(4) * Binomial$new(size = 5, prob = 0.9)$pdf(3))
  expect_equal(ProductDistribution$new(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9))$cdf(x1=4,x2=8),
               Binomial$new(size = 40, prob = 0.2)$cdf(4) * Binomial$new(size = 5, prob = 0.9)$cdf(8))
})
