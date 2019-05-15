library(testthat)

context("Wrappers")

test_that("constructors",{
  expect_error(DistributionWrapper$new())
  expect_error(ConcreteWrapper$new())
})

test_that("parameters",{
  truncbin = truncate(Binomial$new(),1,5)
  expect_error(truncbin$getParameterValue())
  expect_warning(truncbin$getParameterValue("Binom_prob"))
  expect_equal(truncbin$getParameterValue("prob"),0.5)
  expect_silent(truncbin$setParameterValue(list(prob = 0.8)))
})
