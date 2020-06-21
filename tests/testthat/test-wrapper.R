library(testthat)

context("Wrappers")

test_that("constructors", {
  expect_error(DistributionWrapper$new(), "abstract")
})

test_that("parameters", {
  truncbin <- truncate(Binomial$new(), 1, 5)
  expect_condition(truncbin$getParameterValue())
  expect_silent(truncbin$getParameterValue("Binom_prob"))
  expect_equal(truncbin$getParameterValue("Binom_prob"), 0.5)
  expect_silent(truncbin$setParameterValue(Binom_prob = 0.8))
})

test_that("wrapped models", {
  mix <- MixtureDistribution$new(list(Binomial$new(), Normal$new()))
  expect_equal(mix$wrappedModels("Binom"), Binomial$new())
  expect_equal(mix$wrappedModels(), list(Binom = Binomial$new(), Norm = Normal$new()))
  expect_error(mix$wrappedModels("sdsd"), "No distribution called")
  expect_equal(mix$wrappedModels(c("Binom", "Norm")), list(Binom = Binomial$new(),
                                                           Norm = Normal$new()))
})

test_that("wrap a wrapper", {
  expect_silent(
    ProductDistribution$new(list(
      MixtureDistribution$new(list(
        Exponential$new(),
        huberize(truncate(Normal$new(), lower = -10, upper = 10), -5, 5)
      )),
      VectorDistribution$new(distribution = "Gompertz", params = list(
        list(shape = 2, scale = 4),
        list(shape = 1, scale = 5)
      ))
    ))
  )
  x <- MixtureDistribution$new(list(
    Exponential$new(),
    huberize(truncate(Normal$new(), lower = -10, upper = 10), -5, 5)
  ))
  expect_silent(x$parameters())
  expect_silent(x$cdf(2:3, 3:4))
  expect_silent(x$setParameterValue(Exp_rate = 2))
  expect_equal(x$getParameterValue("Exp_rate"), 2)
  expect_silent(x$setParameterValue(HubTruncNorm_TruncNorm_trunc_upper = 6))
  expect_equal(x$getParameterValue("HubTruncNorm_TruncNorm_trunc_upper"), 6)
})
