library(testthat)

context("Wrappers")

test_that("constructors",{
  expect_error(DistributionWrapper$new())
  expect_error(ConcreteWrapper$new())
})

test_that("parameters",{
  truncbin = truncate(Binomial$new(),1,5)
  expect_condition(truncbin$getParameterValue())
  expect_silent(truncbin$getParameterValue("Binom_prob"))
  expect_equal(truncbin$getParameterValue("Binom_prob"),0.5)
  expect_silent(truncbin$setParameterValue(Binom_prob = 0.8))
})

test_that("wrapped models",{
  mix = MixtureDistribution$new(list(Binomial$new(),Normal$new()))
  expect_equal(mix$wrappedModels("Binom"),Binomial$new())
  expect_equal(mix$wrappedModels(), list(Binom = Binomial$new(),Norm = Normal$new()))
  expect_equal(mix$wrappedModels("sdsd"), list(Binom = Binomial$new(),Norm = Normal$new()))
  expect_equal(mix$wrappedModels(c("Binom","Norm")), list(Binom=Binomial$new(),Norm=Normal$new()))
})

test_that("unique parameters",{
  vec = VectorDistribution$new(list(Normal$new(),Normal$new()))
  expect_message(expect_null(vec$getParameterValue("Norm1_mean")))
})


test_that("wrap a wrapper",{
  expect_silent(VectorDistribution$new(
    list(
      ProductDistribution$new(list(
        MixtureDistribution$new(list(
          Exponential$new(),
          huberize(truncate(Normal$new(),lower = -10, upper = 10),-5,5)
          )),
        VectorDistribution$new(distribution = "Gompertz", params=list(list(shape = 2, scale = 4),
                                 list(shape = 1, scale = 5)))
        )),
      Binomial$new()
      )))
  x = ProductDistribution$new(list(MixtureDistribution$new(list(Exponential$new(),
                                                                  huberize(truncate(Normal$new(),lower = -10, upper = 10),-5,5))),
                                                                    Binomial$new()))
  expect_silent(x$parameters())
  expect_silent(x$cdf(2:3,3:4))
  expect_warning(x$pdf(2,1))
  expect_error(x$cdf(2))
  expect_silent(x$setParameterValue(Binom_size = 15))
  expect_equal(x$getParameterValue("Binom_size"), 15)
  expect_silent(x$setParameterValue(lst = list(ExpMixHubTruncNorm_HubTruncNorm_TruncNorm_Norm_var = 7.6)))
  expect_equal(x$getParameterValue("ExpMixHubTruncNorm_HubTruncNorm_TruncNorm_Norm_var"), 7.6)
})
