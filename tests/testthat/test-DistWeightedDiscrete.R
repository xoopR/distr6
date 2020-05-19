library(testthat)

context("WeightedDiscrete distribution")

test_that("autotest", {
  autotest_sdistribution(WeightedDiscrete)
})

test_that("constructor", {
  expect_silent(WeightedDiscrete$new(data = data.frame(x = 1, pdf = 1, cdf = 1)))
  expect_error(WeightedDiscrete$new(data = data.frame(x = 1, pdf = 2, cdf = 1)))
  expect_error(WeightedDiscrete$new(data.frame(x = 1, pdf = 1, cdfs = 1)))
  expect_error(WeightedDiscrete$new(data.frame(x = 1)))
  expect_error(WeightedDiscrete$new(data.frame(xs = 1)))
  expect_null(WeightedDiscrete$new(data.frame(x = 1, pdf = 1, cdf = 1))$getParameterValue(1))
  expect_message(expect_null(WeightedDiscrete$new(data.frame(x = 1, pdf = 1, cdf = 1))$setParameterValue(1)))
})

gd <- WeightedDiscrete$new(data.frame(x = 1:10, pdf = rep(0.1, 10)))
test_that("properties & traits", {
  expect_equal(gd$valueSupport, "discrete")
  expect_equal(gd$variateForm, "univariate")
  expect_equal(gd$symmetry, "asymmetric")
})


test_that("statistics", {
  expect_equal(gd$mean(), mean(1:10))
  expect_equal(WeightedDiscrete$new(data.frame(x = 1:3, pdf = c(0.1, 0.7, 0.2)))$variance(), 0.29)
  expect_equal(gd$variance(), var(1:10) * 9 / 10)
  expect_equal(gd$skewness(), 0)
  expect_equal(round(gd$kurtosis(), 3), -1.224)
  expect_equal(round(gd$kurtosis(F), 3), 1.776)
  expect_equal(round(gd$entropy(), 3), 3.322)
  expect_equal(gd$mgf(1:2), c(3484.377, 56110211))
  expect_equal(gd$mgf(2), 56110211)
  expect_equal(gd$pgf(1:3), c(1, 204.6, 8857.2))
  expect_equal(gd$pgf(2), 204.6)
  expect_equal(round(gd$cf(1), 2), -0.14 + 0.14i)
  expect_equal(round(gd$cf(1:2), 2), round(c(gd$cf(1), gd$cf(2)), 2))
  expect_equal(gd$mode(), 1:10)
  expect_equal(gd$mode(which = 2), 2)
  expect_equal(gd$pdf(2), 1 / 10)
  expect_equal(WeightedDiscrete$new(data.frame(
    x = 1:3,
    cdf = c(1 / 5, 4 / 5, 1)
  ))$pdf(1:3), c(1 / 5, 3 / 5, 1 / 5))
  expect_equal(WeightedDiscrete$new(data.frame(
    x = 1:2,
    pdf = c(0.5, 0.5)
  ))$cdf(1:2), c(0.5, 1))

  gd <- WeightedDiscrete$new(data.frame(x = 1:10, pdf = rep(0.1, 10)))
  expect_equal(round(gd$quantile(gd$cdf(c(2, 5, 6, 4, 1, 30, 20, 2, 2, 2)))), c(2, 5, 6, 4, 1, 10, 10, 2, 2, 2))
  expect_equal(length(gd$rand(10)), 10)
})
