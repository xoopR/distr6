library(testthat)

context("Convolution")

test_that("continuous add", {
  Exp1 = Exponential$new(rate = 1)
  Exp2 = Exponential$new(rate = 1)
  ConvE12 = Convolution$new(Exp1, Exp2)
  decorate(ConvE12, CoreStatistics)
  expect_equal(ConvE12$pdf(1), dgamma(x = 1, shape = 2))
  expect_equal(ConvE12$mean(), 2)
  expect_equal(ConvE12$variance(), 2)
  expect_equal(ConvE12$mean(), Exp1$mean() + Exp2$mean())
  expect_equal(ConvE12$variance(), Exp1$variance() + Exp2$variance())
})

# test_that("continuous subtract", {
#   Exp1 = Exponential$new(rate = 3)
#   Exp2 = Exponential$new(rate = 2)
#   ConvE12 = Convolution$new(Exp1, Exp2, add = FALSE)
#   decorate(ConvE12, CoreStatistics)
#   expect_equal(ConvE12$pdf(1), dgamma(x = 1, shape = 1))
#   expect_equal(ConvE12$mean(), 2)
#   expect_equal(ConvE12$variance(), 2)
#   expect_equal(ConvE12$mean(), Exp1$mean() + Exp2$mean())
#   expect_equal(ConvE12$variance(), Exp1$variance() + Exp2$variance())
# })

test_that("discrete add",{
  Bern1 = Bernoulli$new(prob = 0.1)
  Bern2 = Bernoulli$new(prob = 0.1)
  ConvB12 = Bern1 + Bern2
  decorate(ConvB12, CoreStatistics)
  expect_equal(ConvB12$pdf(1), dbinom(x = 1, size = 2, prob = 0.1))
  expect_equal(ConvB12$mean(), Bern1$mean() + Bern2$mean())
  expect_equal(ConvB12$variance(), Bern1$variance() + Bern1$variance())
})
