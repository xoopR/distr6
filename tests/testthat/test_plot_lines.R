library(testthat)

test_that("valueSupport/variateForm", {
  plot(Normal$new())
  expect_error(lines(MultivariateNormal$new()))
  expect_error(lines(MixtureDistribution$new(list(Binomial$new(), Normal$new()))))
})

test_that("missing d/p/q", {
  plot(Normal$new())
  expect_error(expect_message(
    lines(Distribution$new("s", pdf = function(x) x, type = Reals$new()), fun = "cdf"),
    "does not have a cdf expression"
  ), "No plottable")
  expect_error(expect_message(
    lines(Distribution$new("s", cdf = function(x) x, type = Reals$new()), fun = "pdf"),
    "does not have a pdf expression"
  ), "No plottable")
  expect_error(expect_message(
    lines(Distribution$new("s", pdf = function(x) x, type = Reals$new()), fun = "quantile"),
    "does not have a quantile expression"
  ), "No plottable")
})

test_that("errors", {
  plot(Normal$new())
  expect_error(lines(Binomial$new()), 'argument "fun" is missing')
  expect_error(lines(Binomial$new(), fun = "los"), "Function unrecognised")
})

test_that("silent", {
  plot(Normal$new())
  expect_message(lines(Normal$new(), c("pdf", "cdf")), "Only the first")
  expect_silent(lines(Binomial$new(), "pdf"))
  expect_silent(lines(Binomial$new(), "cdf"))
  expect_silent(lines(Binomial$new(), "quantile"))
  expect_silent(lines(Binomial$new(), "hazard"))
  expect_silent(lines(Binomial$new(), "cumhazard"))
  expect_silent(lines(Geometric$new(), "survival"))

  expect_silent(lines(Normal$new(), "pdf"))
  expect_silent(lines(Normal$new(), "cdf"))
  expect_silent(lines(Normal$new(), "quantile"))
  expect_silent(lines(Normal$new(), "hazard"))
  expect_silent(lines(Normal$new(), "cumhazard"))
  expect_silent(lines(Normal$new(), "survival"))
})
