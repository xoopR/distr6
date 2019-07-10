library(testthat)

context("Vector Distribution")

test_that("constructor",{
  expect_silent(VectorDistribution$new(list(Binomial$new(),Binomial$new(size = 20, prob = 0.6))))
  expect_silent(VectorDistribution$new(list(Binomial$new(),Exponential$new(rate=1))))
})

test_that("type/support",{
  expect_equal(VectorDistribution$new(list(Binomial$new(), Binomial$new()))$type()$getSymbol(), Naturals$new(2)$getSymbol())
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 2), Binomial$new(size = 3)))$support()$getSymbol(),
               product.SetInterval(Set$new(0:2),Set$new(0:3))$getSymbol())
})

test_that("pdf/cdf/quantile",{
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$pdf(4,8),
               data.table::data.table(Binom1 = dbinom(4,40,0.2), Binom2 = dbinom(8,5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$cdf(2:4,1:3),
               data.table::data.table(Binom1 = pbinom(2:4,40,0.2), Binom2 = pbinom(1:3,5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(0.1,0.2),c(0.3,0.4)),
               data.table::data.table(Binom1 = qbinom(c(0.1,0.2),40,0.2), Binom2 = qbinom(c(0.3,0.4),5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(0.1,0.2),c(0.3,0.4),lower.tail = F),
               data.table::data.table(Binom1 = qbinom(c(0.1,0.2),40,0.2,lower.tail = F), Binom2 = qbinom(c(0.3,0.4),5,0.9,lower.tail = F)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(-log(4),-log(5)),c(-log(3),-log(6)),log.p = T),
               data.table::data.table(Binom1 = qbinom(c(-log(4),-log(5)),40,0.2,log.p = T), Binom2 = qbinom(c(-log(3),-log(6)),5,0.9,log.p = T)))
})

test_that("rand",{
  expect_equal(dim(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$rand(5)),
               c(5,2))
})

