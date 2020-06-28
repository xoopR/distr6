library(testthat)

test_that("continuous add", {
  Exp1 <- Exponential$new(rate = 1)
  Exp2 <- Exponential$new(rate = 1)
  expect_equal(round(Convolution$new(Exp1, Exp2)$pdf(1:5), 4), round(dgamma(x = 1:5, shape = 2), 4))
})

test_that("continuous subtract", {
  N1 <- Normal$new(mean = 3)
  N2 <- Normal$new(mean = 2)
  expect_equal(round(Convolution$new(N1, N2, add = FALSE)$pdf(1:5), 4),
               round(dnorm(1:5, 1, sqrt(2)), 4))
})

test_that("discrete add", {
  Bern1 <- Bernoulli$new(prob = 0.1)
  Bern2 <- Bernoulli$new(prob = 0.1)
  ConvB12 <- Bern1 + Bern2
  expect_equal(ConvB12$pdf(1:5), dbinom(1:5, 2, 0.1))
  Geom1 <- Geometric$new(prob = 0.2)
  Geom2 <- Geometric$new(prob = 0.2)
  expect_equal(round(Convolution$new(Geom1, Geom2)$pdf(1:5), 4), round(dnbinom(1:5, 2, 0.2), 4))
})

test_that("discrete subtract", {
  expect_error((Binomial$new() - Binomial$new())$pdf(1), "not currently")
})

test_that("mixed", {
  expect_error(Convolution$new(Binomial$new(), Normal$new()), "mixed distributions")
})
# test_that("discrete subtract", {
#   Binom1 = Binomial$new(size = 5, prob = 0.1)
#   Binom2 = Binomial$new(size = 4, prob = 0.1)
#   ConvBinom = Binom1 - Binom2
#   decorate(ConvBinom,CoreStatistics)
#   ConvBinom$mean()
#   ConvBinom$variance()
#   expect_equal(round(ConvBinom$pdf(1:5),4), round(dbinom(1:5, 10, 0.1),4))
# })
