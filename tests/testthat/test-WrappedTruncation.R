library(testthat)

test_that("check truncation constructor", {
  expect_silent(truncate(Binomial$new(), lower = 1, upper = 5))
  expect_silent(truncate(Binomial$new(), upper = 5))
  expect_silent(truncate(Binomial$new(), lower = 1))
  expect_equal(truncate(Binomial$new())$inf, 1)
  expect_equal(truncate(Binomial$new())$sup, 10)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$inf, 1)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$sup, 10)
  expect_error(TruncatedDistribution$new(MultivariateNormal$new()), "multivariate")
  expect_error(truncate(MixtureDistribution$new(list(Binomial$new(), Normal$new())), 1, 2),
               "mixed")
})

t <- truncate(Binomial$new(), lower = 1, upper = 5)
test_that("pdf", {
  expect_equal(t$pdf(6), 0)
  expect_equal(t$pdf(0), 0)
  expect_equal(
    t$pdf(4),
    dbinom(4, prob = 0.5, size = 10) / ((pbinom(5, prob = 0.5, size = 10) -
      pbinom(1, prob = 0.5, size = 10)))
  )
  expect_equal(
    t$pdf(4, log = TRUE),
    log(dbinom(4, prob = 0.5, size = 10) / ((pbinom(5, prob = 0.5, size = 10) -
                                           pbinom(1, prob = 0.5, size = 10))))
  )
})

test_that("cdf", {
  expect_equal(t$cdf(5), 1)
  expect_equal(t$cdf(6), 1)
  expect_equal(t$cdf(0), 0)
  expec <- (pbinom(4, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10)) /
    (pbinom(5, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10))
  expect_equal(t$cdf(4, log.p = FALSE, lower.tail = TRUE), expec)
  expect_equal(t$cdf(4, log.p = TRUE, lower.tail = TRUE), log(expec))
  expect_equal(t$cdf(4, log.p = FALSE, lower.tail = FALSE), 1 - expec)
  expect_equal(t$cdf(4, log.p = TRUE, lower.tail = FALSE), log(1 - expec))
})

test_that("quantile", {
  expect_equal(t$.__enclos_env__$private$.quantile(-20), 1)
  expect_equal(t$.__enclos_env__$private$.quantile(20), 5)
  expect_equal(t$quantile(0.1), 3)
  r <- expect_silent({t$rand(5)})
  expect_length(r, 5)
  expect_true(all(r >= 1) & all(r <= 5))
})

test_that("strprint", {
  expect_equal(t$properties$support$strprint(), Interval$new(2, 5, class = "integer")$strprint())
  expect_equal(
    truncate(Exponential$new(), lower = 2, upper = 3)$properties$support$strprint(),
    Interval$new(2, 3, type = "(]")$strprint()
  )
})


test_that("check missing", {
  expect_error(truncate(Distribution$new("Test", pdf = dnorm), 1, 2))
  expect_error(truncate(Distribution$new("Test", cdf = pnorm), 1, 2))
})

test_that("check truncation parameters", {
  x <- truncate(Binomial$new(), lower = 1, upper = 5)
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 5)
  expect_equal(x$getParameterValue("trunc_lower"), 1)
  expect_equal(x$getParameterValue("trunc_upper"), 5)
  expect_silent(x$setParameterValue(trunc_lower = 2))
  expect_silent(x$setParameterValue(trunc_upper = 10))
  expect_equal(x$inf, 3)
  expect_equal(x$sup, 10)
  expect_equal(x$getParameterValue("trunc_lower"), 2)
  expect_equal(x$getParameterValue("trunc_upper"), 10)
  expect_error(x$setParameterValue(trunc_upper = 1))
  expect_error(x$setParameterValue(trunc_lower = 12))
  expect_error(x$setParameterValue(trunc_lower = 4, trunc_upper = 3))
  x <- truncate(Exponential$new(), lower = 1, upper = 5)
  expect_silent(x$setParameterValue(trunc_lower = 2, trunc_upper = 10))
  expect_equal(x$inf, 2)
  expect_equal(x$sup, 10)
  expect_true(testInterval(x$properties$support))
})

test_that("missing pdf/cdf", {
  expect_error(truncate(Distribution$new("a", pdf = function(x) x, type = Reals$new())),
               "pdf and cdf")
})
