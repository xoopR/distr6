library(testthat)

context("plot")

test_that("valueSupport/variateForm", {
  expect_error(plot(MultivariateNormal$new()))
  expect_error(plot(MixtureDistribution$new(list(Binomial$new(), Normal$new()))))
})

test_that("missing d/p/q", {
  expect_error(expect_message(
    plot(Distribution$new("s", pdf = function(x) x), fun = "cdf"),
    "does not have a cdf expression"
  ), "No plottable")
  expect_error(expect_message(
    plot(Distribution$new("s", cdf = function(x) x), fun = "pdf"),
    "does not have a pdf expression"
  ), "No plottable")
  expect_error(expect_message(
    plot(Distribution$new("s", pdf = function(x) x), fun = "quantile"),
    "does not have a quantile expression"
  ), "No plottable")
})

test_that("errors", {
  expect_error(plot(Binomial$new(), fun = "los"), "Function unrecognised")
})

test_that("silent", {
  expect_silent(plot(Binomial$new(), "all"))
  expect_silent(plot(Binomial$new()))
  expect_silent(plot(Binomial$new(), "pdf"))
  expect_silent(plot(Binomial$new(), c("pdf", "cdf", "quantile")))
  expect_silent(plot(Binomial$new(), plot = FALSE))
  expect_silent(plot(Binomial$new(), arrange = FALSE))
  expect_silent(plot(Normal$new(), "all"))
  expect_silent(plot(Normal$new()))
  expect_silent(plot(Normal$new(), "pdf"))
  expect_silent(plot(Normal$new(), plot = FALSE))
  expect_silent(plot(Normal$new(), arrange = FALSE))
  expect_silent(plot(Geometric$new(), "all"))
})

test_that("structure", {
  expect_equal(
    plot(Binomial$new(), "cdf", npoints = 1, plot = FALSE)[, 1:2],
    data.table::data.table(points = 0:10, cdf = pbinom(0:10, 10, 0.5))
  )
})

test_that("pars", {
  expect_silent(plot(Geometric$new(),
    fun = "all", ask = TRUE, pdf_col = 1, cdf_col = 2,
    quantile_col = 3, hazard_col = 4, cumhazard_col = 5, survival_col = 6
  ))
  expect_silent(plot(Normal$new(),
    fun = "all", ask = TRUE, pdf_col = 1, cdf_col = 2,
    quantile_col = 3, hazard_col = 4, cumhazard_col = 5, survival_col = 6
  ))
})

test_that("use_rand", {
  expect_silent(plot(Wald$new(), fun = "cdf"))
})

dexpo <- function(x) {
  m1 <- self$getParameterValue("rate")
  m2 <- exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}
cexpo <- function(x) {
  m1 <- exp(-1 * self$getParameterValue("rate") * x)
  return(1 - m1)
}

ps <- ParameterSet$new(
  id = list("rate", "scale", "test"), value = list(1, 1, 0),
  support = list(PosReals$new(zero = T), PosReals$new(zero = T), Reals$new()),
  settable = list(TRUE, FALSE, FALSE),
  updateFunc = list(
    NULL,
    function(self) 1 / self$getParameterValue("rate"),
    function(self) exp(self$getParameterValue("rate"))
  ),
  description = list("Arrival rate", "Scale parameter", "testpar")
)

continuousTester <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  pdf = dexpo,
  cdf = cexpo,
  parameters = ps
)
test_that("use_support", {
  expect_message(plot(continuousTester, xlim = c(0, 10)), "No quantile or")
})
