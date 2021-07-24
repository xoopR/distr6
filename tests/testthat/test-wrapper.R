test_that("constructors", {
  expect_error(DistributionWrapper$new(), "abstract")
})

test_that("parameters", {
  truncbin <- truncate(Binomial$new(), 1, 5)
  expect_condition(truncbin$getParameterValue())
  expect_equal(truncbin$getParameterValue("Binom__prob"), 0.5)
  expect_silent(truncbin$setParameterValue(Binom__prob = 0.8))
})

test_that("wrapped models", {
  conv <- Convolution$new(Binomial$new(), Geometric$new())
  expect_equal(conv$wrappedModels(), list(Binom = Binomial$new(), Geom = Geometric$new()))
  expect_equal(conv$wrappedModels("dfdsf"), list(Binom = Binomial$new(), Geom = Geometric$new()))
  expect_equal(conv$wrappedModels(c("Binom", "Geom")),
               list(Binom = Binomial$new(), Geom = Geometric$new()))
  expect_equal(conv$wrappedModels("Binom"), Binomial$new())
})

test_that("wrap a wrapper", {
  exp <- Exponential$new()
  norm <- Normal$new()
  bin <- Binomial$new()
  trunc <- truncate(norm, lower = -10, upper = 10)
  mix <- MixtureDistribution$new(list(exp, trunc))
  prod <- ProductDistribution$new(list(mix, bin))
  expect_distribution(trunc, "TruncatedDistribution")
  expect_distribution(mix, "MixtureDistribution")
  expect_distribution(prod, "ProductDistribution")

  hub <- huberize(trunc, -5, 5)
  expect_distribution(hub, "HuberizedDistribution")

  x <- MixtureDistribution$new(list(exp, hub))
  expect_distribution(x, "MixtureDistribution")

  expect_silent(x$parameters())
  expect_silent(x$cdf(2:3, 3:4))
  expect_silent(x$setParameterValue(Exp__rate = 2))
  expect_equal(x$getParameterValue("Exp__rate"), 2)
  expect_silent(x$setParameterValue(HubTruncNorm__TruncNorm__trunc__upper = 6))
  expect_equal(x$getParameterValue("HubTruncNorm__TruncNorm__trunc__upper"), 6)
  expect_equal(x$getParameterValue("upper"), list(
    HubTruncNorm__TruncNorm__trunc__upper = 6,
    HubTruncNorm__hub__upper = 5
  ))
})
