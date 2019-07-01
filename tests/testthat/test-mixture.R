library(testthat)

context("Mixture")

test_that("check weights", {
  expect_equal(MixtureDistribution$new(list(Exponential$new(),Normal$new()))$.__enclos_env__$private$.weights,
               c(0.5,0.5))
  expect_equal(MixtureDistribution$new(list(Binomial$new(),Exponential$new(),Normal$new()))$.__enclos_env__$private$.weights,
               c(1/3,1/3,1/3))
  expect_equal(MixtureDistribution$new(list(Binomial$new(),Exponential$new(),Normal$new()),
                                       weights = c(0.1,0.6,0.3))$.__enclos_env__$private$.weights,
               c(0.1,0.6,0.3))
})

test_that("check pdf", {
  M <- MixtureDistribution$new(list(Binomial$new(),Exponential$new(),Normal$new()),weights = c(0.1,0.6,0.3))
  expect_equal(M$pdf(1), Binomial$new()$pdf(1)*0.1 + Exponential$new()$pdf(1)*0.6 + Normal$new()$pdf(1)*0.3)
})

test_that("check cdf", {
  M <- MixtureDistribution$new(list(Binomial$new(),Exponential$new(),Normal$new()),weights = c(0.1,0.6,0.3))
  expect_equal(M$cdf(1), Binomial$new()$cdf(1)*0.1 + Exponential$new()$cdf(1)*0.6 + Normal$new()$cdf(1)*0.3)
})
