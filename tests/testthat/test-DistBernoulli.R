library(testthat)

context("Bernoulli distribution")

test_that("autottest", {
  autotest_sdistribution(Bernoulli)
})

test_that("constructor", {
  expect_silent(Bernoulli$new())
  expect_silent(Bernoulli$new(prob = 0.2))
  expect_silent(Bernoulli$new(qprob = 0.2))
  expect_message(Bernoulli$new(qprob = 0.2, verbose = T))
  expect_equal(Bernoulli$new(prob = 0.2)$getParameterValue("qprob"), 0.8)
  expect_equal(Bernoulli$new(qprob = 0.2)$getParameterValue("prob"), 0.8)
})

test_that("properties & traits", {
  expect_equal(Bernoulli$new()$valueSupport, "discrete")
  expect_equal(Bernoulli$new()$variateForm, "univariate")
  expect_equal(Bernoulli$new()$symmetry, "asymmetric")
  expect_equal(Bernoulli$new()$sup, 1)
  expect_equal(Bernoulli$new()$inf, 0)
  expect_equal(Bernoulli$new()$dmax, 1)
  expect_equal(Bernoulli$new()$dmin, 0)
})


b <- Bernoulli$new(prob = 0.2)
test_that("statistics", {
  expect_equal(b$mean(), 0.2)
  expect_equal(b$variance(), 0.2 * 0.8)
  expect_equal(b$skewness(), 1.5)
  expect_equal(b$kurtosis(T), 0.25)
  expect_equal(b$kurtosis(F), 3.25)
  expect_equal(round(b$entropy(), 5), 0.72193)
  expect_equal(b$mgf(1), 0.8 + 0.2 * exp(1))
  expect_equal(b$cf(1), 0.8 + 0.2 * exp(1i))
  expect_equal(b$pgf(1), 1)
  expect_equal(b$mode(), 0)
  expect_equal(Bernoulli$new(prob = 0.5)$mode(), c(0, 1))
  expect_equal(Bernoulli$new(prob = 0.5)$mode(2), 1)
  expect_equal(Bernoulli$new(prob = 0.8)$mode(), 1)
  expect_equal(b$pdf(1), dbinom(1, 1, 0.2))
  expect_equal(b$cdf(1), pbinom(1, 1, 0.2))
  expect_equal(b$quantile(0.324), qbinom(0.324, 1, 0.2))
  expect_equal(length(b$rand(10)), 10)
})
