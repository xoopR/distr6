library(testthat)

context("Truncation")

test_that("check truncation constructor",{
  expect_silent(truncate(Binomial$new(),lower = 1, upper = 5))
  expect_silent(truncate(Binomial$new(), upper = 5))
  expect_silent(truncate(Binomial$new(),lower = 1))
  expect_equal(truncate(Binomial$new())$inf(), 0)
  expect_equal(truncate(Binomial$new())$sup(), 10)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$inf(), 0)
  expect_equal(truncate(Binomial$new(), lower = -Inf, upper = Inf)$sup(), 10)
})

test_that("truncation results",{
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$pdf(6), 0)
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$pdf(0), 0)
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$pdf(4),
               dbinom(4, prob = 0.5, size = 10)/((pbinom(5,prob=0.5,size=10)-pbinom(1,prob=0.5,size=10))))
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$cdf(6), 1)
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$cdf(0), 0)
  expect_equal(truncate(Binomial$new(),lower = 1, upper = 5)$cdf(4),
               (pbinom(4, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10))/
                 (pbinom(5, prob = 0.5, size = 10) - pbinom(1, prob = 0.5, size = 10)))
})


test_that("check missing",{
  expect_error(truncate(Distribution$new("Test", pdf = dnorm),1,2))
  expect_error(truncate(Distribution$new("Test", cdf = pnorm),1,2))
})

