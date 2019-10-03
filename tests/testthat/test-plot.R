library(testthat)

context("plot")

test_that("valueSupport/variateForm",{
  expect_error(plot(MultivariateNormal$new()))
  expect_error(plot(MixtureDistribution$new(list(Binomial$new(),Normal$new()))))
})

test_that("missing d/p/q",{
  expect_error(expect_message(plot(Distribution$new("s", pdf = function(x) x), fun = "cdf"),
                 "does not have a cdf expression"))
  expect_error(expect_message(plot(Distribution$new("s", cdf = function(x) x), fun = "pdf"),
                              "does not have a pdf expression"))
  expect_error(expect_message(plot(Distribution$new("s", pdf = function(x) x), fun = "quantile"),
                              "does not have a quantile expression"))
})

test_that("errors",{
  expect_error(plot(Binomial$new(), fun = "los"), 'Function unrecognised')
})

test_that("silent",{
  expect_silent(plot(Binomial$new(), "all"))
  expect_silent(plot(Binomial$new()))
  expect_silent(plot(Binomial$new(), "pdf"))
  expect_silent(plot(Binomial$new(), c("pdf","cdf","quantile")))
  expect_silent(plot(Binomial$new(), plot = FALSE))
  expect_silent(plot(Binomial$new(), arrange = FALSE))
  expect_silent(plot(Normal$new(), "all"))
  expect_silent(plot(Normal$new()))
  expect_silent(plot(Normal$new(), "pdf"))
  expect_silent(plot(Normal$new(), plot = FALSE))
  expect_silent(plot(Normal$new(), arrange = FALSE))
  expect_silent(plot(Geometric$new(), "all"))
})

test_that("structure",{
  expect_equal(plot(Binomial$new(), "pdf", npoints = 1, plot = FALSE)[,1:2],
               data.table::data.table(points = 0:10, cdf = pbinom(0:10,10,0.5)))
})

test_that("pars",{
  expect_silent(plot(Geometric$new(), fun = "all", ask = TRUE, pdf_col = 1, cdf_col = 2,
          quantile_col = 3, hazard_col = 4, cumhazard_col = 5, survival_col = 6))
  expect_silent(plot(Normal$new(), fun = "all", ask = TRUE, pdf_col = 1, cdf_col = 2,
                     quantile_col = 3, hazard_col = 4, cumhazard_col = 5, survival_col = 6))
})
