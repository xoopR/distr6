library(testthat)

context("simulateEmpiricalDistribution")

test_that("simulateEmpiricalDistribution",{
  emp <- Empirical$new(stats::runif(1000))
  expect_equal(simulateEmpiricalDistribution(emp, 100, seed = 42),
               simulateEmpiricalDistribution(emp, 100, seed = 42))
  expect_error(simulateEmpiricalDistribution(Binomial$new(), 100))
  expect_silent(simulateEmpiricalDistribution(emp, 100))
  expect_equal(length(simulateEmpiricalDistribution(emp, 1:10)), 10)
  expect_equal(length(simulateEmpiricalDistribution(Empirical$new(stats::runif(10)), 100)), 10)
})
