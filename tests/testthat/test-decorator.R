library(testthat)

context("Decorator")

test_that("abstract decorator",{
  expect_error(DistributionDecorator$new(Binomial$new()))
  expect_error(DistributionDecorator$new())
})

test_that("decorate",{
  expect_message(decorate(Exponential$new(),CoreStatistics, R62S3 = F))
  expect_error(decorate(Exponential$new(),CoreStatistics,R62S3 = T))
  y = Exponential$new(decorators = CoreStatistics, R62S3 = F)
  expect_equal(decorate(y,CoreStatistics),"y is already decorated with CoreStatistics")
})