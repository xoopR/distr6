library(testthat)

context("Huberization")

test_that("check continuous Huberized wrapper", {
  hubExp <- HuberizedDistribution$new(Exponential$new(), lower = 2, upper = 5)
  expect_equal(hubExp$cdf(1), 0)
  expect_equal(hubExp$cdf(2), Exponential$new()$cdf(2))
  expect_equal(hubExp$cdf(4), Exponential$new()$cdf(4))
  expect_equal(hubExp$cdf(100), 1)
  expect_equal(hubExp$cdf(5), 1)
  expect_equal(length(hubExp$rand(10)), 10)
  expect_equal(hubExp$quantile(0), 2)
  expect_equal(hubExp$quantile(1), 5)
  expect_equal(hubExp$quantile(0.987), Exponential$new()$quantile(0.987))
})

test_that("check discrete Huberized wrapper", {
  hubBin <- HuberizedDistribution$new(Binomial$new(), lower = 2, upper = 5)
  expect_equal(hubBin$pdf(1), 0)
  expect_equal(hubBin$pdf(2), Binomial$new()$cdf(2))
  expect_equal(hubBin$pdf(5), Binomial$new()$cdf(5, lower.tail = F) + Binomial$new()$pdf(5))
  expect_equal(hubBin$pdf(3), Binomial$new()$pdf(3))
  expect_equal(hubBin$quantile(0.9), 5)
  expect_equal(hubBin$quantile(log(1 - 0.9), log.p = T, lower.tail = F), 5)
})

test_that("check huberization constructor", {
  expect_silent(huberize(Binomial$new(), lower = 1, upper = 5))
  expect_equal(huberize(Binomial$new(), lower = -5, upper = 20)$getParameterValue("hub_lower"), 0)
  expect_equal(huberize(Binomial$new(), lower = -5, upper = 20)$getParameterValue("hub_upper"), 10)
  expect_silent(huberize.Distribution(Binomial$new(), upper = 5))
  expect_silent(huberize(Binomial$new(), lower = 1))
  expect_silent(huberize(Binomial$new()))
  expect_error(huberize(Distribution$new("Test", pdf = dbinom), 1, 2))
  expect_error(huberize(MixtureDistribution$new(list(Binomial$new(), Normal$new())), 1, 2),
               "mixed")
  expect_error(huberize(MultivariateNormal$new()), "multivariate")
})

test_that("check huberization parameters", {
  x <- huberize(Binomial$new(), lower = 1, upper = 5)
  expect_equal(x$inf, 1)
  expect_equal(x$sup, 5)
  expect_equal(x$getParameterValue("hub_lower"), 1)
  expect_equal(x$getParameterValue("hub_upper"), 5)
  expect_silent(x$setParameterValue(hub_lower = 2))
  expect_silent(x$setParameterValue(hub_upper = 10))
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 10)
  expect_equal(x$getParameterValue("hub_lower"), 2)
  expect_equal(x$getParameterValue("hub_upper"), 10)
  expect_error(x$setParameterValue(hub_upper = 1))
  expect_error(x$setParameterValue(hub_lower = 12))
  expect_error(x$setParameterValue(hub_lower = 4, hub_upper = 3))
  expect_true(testInterval(x$properties$support))
  x <- huberize(Exponential$new(), lower = 1, upper = 5)
  expect_silent(x$setParameterValue(hub_lower = 2, hub_upper = 10))
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 10)
  expect_true(testInterval(x$properties$support))
})

test_that("missing pdf/cdf", {
  expect_error(huberize(Distribution$new("a", pdf = function(x) x, type = Reals$new())),
               "pdf and cdf")
})
