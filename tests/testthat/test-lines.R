library(testthat)

context("lines")

test_that("valueSupport/variateForm",{
  plot(Normal$new())
  expect_error(lines(MultivariateNormal$new()))
  expect_error(lines(MixtureDistribution$new(list(Binomial$new(),Normal$new()))))
})

test_that("missing d/p/q",{
  plot(Normal$new())
  expect_error(expect_message(lines(Distribution$new("s", pdf = function(x) x), fun = "cdf"),
                              "does not have a cdf expression"))
  expect_error(expect_message(lines(Distribution$new("s", cdf = function(x) x), fun = "pdf"),
                              "does not have a pdf expression"))
  expect_error(expect_message(lines(Distribution$new("s", pdf = function(x) x), fun = "quantile"),
                              "does not have a quantile expression"))
})
