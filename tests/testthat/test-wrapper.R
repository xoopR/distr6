library(testthat)

context("Wrappers")

test_that("constructors",{
  expect_error(DistributionWrapper$new())
  expect_error(ConcreteWrapper$new())
})

test_that("parameters",{
  truncbin = truncate(Binomial$new(),1,5)
  expect_condition(truncbin$getParameterValue())
  expect_warning(truncbin$getParameterValue("Binom_prob"))
  expect_equal(truncbin$getParameterValue("prob"),0.5)
  expect_silent(truncbin$setParameterValue(prob = 0.8))
})

test_that("wrapped models",{
  vec = VectorDistribution$new(list(Binomial$new(),Normal$new()))
  expect_equal(vec$wrappedModels("Binom"),Binomial$new())
  expect_equal(vec$wrappedModels(c("Binom","Norm")), list(Binom=Binomial$new(),Norm=Normal$new()))
  expect_equal(vec$wrappedModels("asds"),list(Binom=Binomial$new(),Norm=Normal$new()))
})

test_that("unique parameters",{
  vec = VectorDistribution$new(list(Normal$new(),Normal$new()))
  expect_silent(vec$getParameterValue("Norm1_mean"))
  expect_silent(vec$getParameterValue("Norm2_mean"))
  expect_equal(vec$setParameterValue(Norm1_var = 2)$getParameterValue("Norm1_prec"),1/2)
  expect_equal(vec$setParameterValue(Norm2_sd = 2)$getParameterValue("Norm2_var"),4)
})
