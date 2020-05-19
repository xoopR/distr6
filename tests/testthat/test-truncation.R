library(testthat)

context("Truncation")

test_that("check truncation constructor", {
  expect_silent(truncate(Binomial$new(), lower = 1, upper = 5))
  expect_silent(truncate(Binomial$new(), upper = 5))
  expect_silent(truncate(Binomial$new(), lower = 1))
  expect_equal(truncate(Binomial$new())$inf, 0)
  expect_equal(truncate(Binomial$new())$sup, 10)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$inf, 0)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$sup, 10)
})

t <- truncate(Binomial$new(), lower = 1, upper = 5)
test_that("truncation results", {
  expect_equal(t$pdf(6), 0)
  expect_equal(t$pdf(0), 0)
  expect_equal(
    t$pdf(4),
    dbinom(4, prob = 0.5, size = 10) / ((pbinom(5, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10)))
  )
  expect_equal(t$cdf(5), 1)
  expect_equal(t$cdf(6), 1)
  expect_equal(t$cdf(0), 0)
  expect_equal(
    t$cdf(4),
    (pbinom(4, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10)) /
      (pbinom(5, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10))
  )
  expect_equal(t$support$strprint(), Interval$new(1, 5, class = "integer")$strprint())
  expect_equal(truncate(Exponential$new(), lower = 2, upper = 3)$support$strprint(), Interval$new(2, 3)$strprint())
})


test_that("check missing", {
  expect_error(truncate(Distribution$new("Test", pdf = dnorm), 1, 2))
  expect_error(truncate(Distribution$new("Test", cdf = pnorm), 1, 2))
})

test_that("check truncation parameters", {
  x <- truncate(Binomial$new(), lower = 1, upper = 5)
  expect_equal(x$inf, 1)
  expect_equal(x$sup, 5)
  expect_equal(x$getParameterValue("truncLower"), 1)
  expect_equal(x$getParameterValue("truncUpper"), 5)
  expect_silent(x$setParameterValue(truncLower = 2))
  expect_silent(x$setParameterValue(truncUpper = 10))
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 10)
  expect_equal(x$getParameterValue("truncLower"), 2)
  expect_equal(x$getParameterValue("truncUpper"), 10)
  expect_error(x$setParameterValue(truncUpper = 1))
  expect_error(x$setParameterValue(truncLower = 12))
  expect_error(x$setParameterValue(truncLower = 4, truncUpper = 3))
  x <- truncate(Exponential$new(), lower = 1, upper = 5)
  expect_silent(x$setParameterValue(truncLower = 2, truncUpper = 10))
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 10)
  expect_true(testInterval(x$support))
})
