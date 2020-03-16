library(testthat)
library(magrittr)

context("Decorator")

test_that("abstract decorator",{
  expect_error(DistributionDecorator$new(Binomial$new()))
  expect_error(DistributionDecorator$new())
})

test_that("decorate",{
  expect_message(decorate(Exponential$new(rate=1),CoreStatistics))
  expect_message(Exponential$new(rate=1) %>% decorate(CoreStatistics))
  expect_message(Exponential$new(rate=1) %>% decorate("CoreStatistics"))
  y = Exponential$new(rate=1,decorators = CoreStatistics)
  expect_message(decorate(y,CoreStatistics),"y is already decorated with CoreStatistics")
  E <- Exponential$new(rate = 1)
  CoreStatistics$new(E)
  expect_equal(E$decorators,"CoreStatistics")
})
