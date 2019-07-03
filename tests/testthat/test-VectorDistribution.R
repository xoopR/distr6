library(testthat)

context("Vector Distribution")

test_that("constructor",{
  expect_silent(VectorDistribution$new(list(Binomial$new(),Binomial$new(size = 20, prob = 0.6))))
  expect_silent(VectorDistribution$new(list(Binomial$new(),Exponential$new(rate=1))))
})

test_that("type/support/distrDomain",{
  expect_equal(VectorDistribution$new(list(Binomial$new(), Binomial$new()))$type()$getSymbol(), Naturals$new(2)$getSymbol())
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 2), Binomial$new(size = 3)))$support()$getSymbol(),
               product.SetInterval(Set$new(0:2),Set$new(0:3))$getSymbol())
  expect_equal(VectorDistribution$new(list(Binomial$new(), Binomial$new()))$distrDomain()$getSymbol(), Naturals$new(2)$getSymbol())
})

test_that("pdf/cdf",{
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$pdf(4,8),
               data.table::data.table(Binom1 = dbinom(4,40,0.2), Binom2 = dbinom(8,5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$cdf(2:4,1:3),
               data.table::data.table(Binom1 = pbinom(2:4,40,0.2), Binom2 = pbinom(1:3,5,0.9)))
})

test_that("rand",{
  expect_equal(dim(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$rand(5)),
               c(5,2))
})

