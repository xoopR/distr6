library(testthat)
library(magrittr)

context("Decorator")

test_that("abstract decorator", {
  expect_error(DistributionDecorator$new(Binomial$new()), "unused")
  expect_error(DistributionDecorator$new(), "abstract")
})

test_that("decorate", {
  expect_message(decorate(Exponential$new(rate = 1), "CoreStatistics"))
  expect_message(Exponential$new(rate = 1) %>% decorate("CoreStatistics"))
  y <- Exponential$new(rate = 1, decorators = "CoreStatistics")
  expect_message(decorate(y, "CoreStatistics"),
                 "Exponential is already decorated with CoreStatistics")
  E <- Exponential$new(rate = 1)
  expect_message(CoreStatistics$new()$decorate(E))
  expect_equal(E$decorators, "CoreStatistics")
  expect_message(decorate(E, "ExoticStatistics"))
  expect_equal(E$decorators, c("CoreStatistics", "ExoticStatistics"))
  expect_message(CoreStatistics$new()$decorate(E),
                 "Exponential is already decorated with CoreStatistics")
})
