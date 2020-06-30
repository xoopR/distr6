library(testthat)

test_that("univariate", {
  emp <- Empirical$new(stats::runif(10))
  expect_equal(
    simulateEmpiricalDistribution(emp, 10, seed = 42),
    simulateEmpiricalDistribution(emp, 10, seed = 42)
  )
  expect_error(simulateEmpiricalDistribution(Binomial$new(), 10))
  expect_silent(simulateEmpiricalDistribution(emp, 20))
  expect_equal(length(simulateEmpiricalDistribution(emp, 1:10)), 10)
  expect_equal(length(simulateEmpiricalDistribution(Empirical$new(stats::runif(10)), 10)), 10)
})

test_that("multivariate", {
  emp <- EmpiricalMV$new(data.frame(stats::runif(10), stats::runif(10)))
  checkmate::expect_data_table(simulateEmpiricalDistribution(emp, 20), nrows = 10, ncols = 2)
})
