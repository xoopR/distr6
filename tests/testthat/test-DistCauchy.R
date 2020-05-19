library(testthat)

context("Cauchy distribution")

test_that("autottest", {
  autotest_sdistribution(Cauchy)
})

test_that("parameterisation", {
  expect_silent(Cauchy$new())
  expect_silent(Cauchy$new(location = 10))
  expect_silent(Cauchy$new(scale = 20))
  expect_error(Cauchy$new(scale = -1))
  expect_equal(Cauchy$new(location = 5)$getParameterValue("location"), 5)
  expect_equal(Cauchy$new(scale = 10)$getParameterValue("scale"), 10)
})

test_that("properties & traits", {
  expect_equal(Cauchy$new()$valueSupport, "continuous")
  expect_equal(Cauchy$new()$variateForm, "univariate")
  expect_equal(Cauchy$new()$symmetry, "symmetric")
  expect_equal(Cauchy$new()$sup, Inf)
  expect_equal(Cauchy$new()$inf, -Inf)
  expect_equal(Cauchy$new()$dmax, Inf)
  expect_equal(Cauchy$new()$dmin, -Inf)
})

c <- Cauchy$new(location = 0, scale = 1)
test_that("statistics", {
  expect_equal(c$mean(), NaN)
  expect_equal(c$variance(), NaN)
  expect_equal(c$skewness(), NaN)
  expect_equal(c$kurtosis(T), NaN)
  expect_equal(c$kurtosis(F), NaN)
  expect_equal(c$entropy(), log(4 * pi, 2))
  expect_equal(c$mgf(0), NaN)
  expect_equal(c$cf(1), as.complex(exp(-1)))
  expect_equal(c$mode(), 0)
  expect_equal(c$pgf(1), NaN)
  expect_equal(c$pdf(1), dcauchy(1))
  expect_equal(c$cdf(1), pcauchy(1))
  expect_equal(c$quantile(0.56), qcauchy(0.56))
  expect_equal(c$cdf(c$quantile(0.56)), 0.56)
  expect_silent(c$rand(10))
})
