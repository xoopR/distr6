library(testthat)

context("Decorator")

test_that("abstract decorator",{
  expect_error(DistributionDecorator$new(Binomial$new()))
  expect_error(DistributionDecorator$new())
})

test_that("decorate",{
  expect_message(decorate(Exponential$new(rate=1),CoreStatistics, R62S3 = F))
  y = Exponential$new(rate=1,decorators = CoreStatistics, R62S3 = F)
  expect_equal(decorate(y,CoreStatistics),"y is already decorated with CoreStatistics")
})
