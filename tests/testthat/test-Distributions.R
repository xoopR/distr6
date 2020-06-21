library(testthat)

dbin <- function(x, log) {
  m1 <- choose(self$getParameterValue("size"), x)
  m2 <- self$getParameterValue("prob")^x
  m3 <- (1 - self$getParameterValue("prob"))^(self$getParameterValue("size") - x) # nolint
  return(m1 * m2 * m3)
}

test_that("check constructor", {
  expect_error(Distribution$new(short_name = "Test_Distr"))
  expect_error(Distribution$new(pdf = dbin))
  expect_error(Distribution$new("Discrete Test", "TestDistr", pdf = dbin), "type must be")
  expect_silent(Distribution$new("Discrete Test", "TestDistr", pdf = dbin, type = Naturals$new()))
  expect_error(Distribution$new(short_name = "Test Distr", pdf = dbin, type = Naturals$new()))
  expect_error(Distribution$new(short_name = "TestDistr", pdf = dbinom, type = Naturals$new()),
               "subset of")
  expect_silent(Distribution$new(short_name = "TestDistr", pdf = dbin, type = Naturals$new()))
  expect_silent(Distribution$new(name = "Test Distr", pdf = dbin, type = Naturals$new()))
  expect_equal(Distribution$new(name = "Test Distr", pdf = dbin, type = Naturals$new())$strprint(),
               "TestDistr")
  expect_null(Distribution$new(name = "Test Distr", pdf = dbin, type = Naturals$new())$parameters())
})


test_that("check support", {
  expect_equal(Distribution$new("Discrete Test", valueSupport = "c", pdf = dbin,
                                type = Naturals$new())$valueSupport, "continuous")
  expect_equal(Distribution$new("Discrete Test", valueSupport = "d", pdf = dbin,
                                type = Naturals$new())$valueSupport, "discrete")
  expect_equal(Distribution$new("Discrete Test", valueSupport = "m", pdf = dbin,
                                type = Naturals$new())$valueSupport, "mixture")
  expect_error(Distribution$new("Discrete Test", valueSupport = "r", pdf = dbin,
                                type = Naturals$new()))
  expect_equal(Distribution$new("Discrete Test", pdf = dbin, type = Naturals$new())$valueSupport,
               "discrete")
})

ps <- ParameterSet$new(
  id = list("prob", "size", "qprob"), value = list(0.2, 100, 0.8),
  support = list(Interval$new(0, 1), PosNaturals$new(), Interval$new(0, 1)),
  settable = list(TRUE, TRUE, FALSE),
  description = list(
    "Probability of Success", "Number of trials",
    "Probability of failure"
  )
)
ps$addDeps(dt = data.table(
  x = c("prob", "qprob"),
  y = c("qprob", "prob"),
  fun = c(
    function(self) 1 - self$getParameterValue("prob"),
    function(self) 1 - self$getParameterValue("qprob")
  )
))

test_that("check r/d/p/q", {
  expect_silent(Distribution$new("Test", pdf = dbin, parameters = ps, type = Naturals$new())$pdf(1))
  expect_silent(Distribution$new("Test",
    pdf = dbin, type = Naturals$new(),
    quantile = function(p) {
      return(p)
    }
  )$quantile(0.4))
  expect_null(Distribution$new("Test", pdf = dbin, type = Naturals$new())$cdf(1))
  expect_null(Distribution$new("Test", pdf = dbin, type = Naturals$new())$quantile(1))
  expect_null(Distribution$new("Test", pdf = dbin, type = Naturals$new())$rand(1))
})

test_that("check is", {
  expect_equal(isPdf(Distribution$new("Test", pdf = dbin, parameters = ps, type = Naturals$new())),
               1L)
  expect_equal(isCdf(Distribution$new("Test", pdf = dbin, parameters = ps, type = Naturals$new())),
               0L)
  expect_equal(isQuantile(Distribution$new("Test", pdf = dbin, parameters = ps,
                                           type = Naturals$new())), 0L)
  expect_equal(isRand(Distribution$new("Test", pdf = dbin, parameters = ps, type = Naturals$new())),
               0L)
})

test_that("working_support", {
  expect_equal(Exponential$new()$workingSupport, Interval$new(0, 100))
  expect_equal(Binomial$new()$workingSupport, Set$new(elements = 0:10, class = "integer"))
  expect_equal(Normal$new()$workingSupport, Interval$new(-100, 10))
})

test_that("print", {
  expect_output(Binomial$new()$print(1))
  expect_output(Binomial$new()$print(5))
})

test_that("suppress", {
  expect_silent(Distribution$new(
    name = "name",
    short_name = "name",
    type = Reals$new(),
    support = Reals$new(),
    symmetric = TRUE,
    pdf = function(x) {
      return(x)
    },
    cdf = function(x) {
      return(x)
    },
    quantile = function(x) {
      return(x)
    },
    rand = function(x) {
      return(x)
    },
    parameters = ps,
    decorators = "CoreStatistics",
    valueSupport = "continuous",
    variateForm = "univariate", description = "test",
    suppressMoments = TRUE, .suppressChecks = TRUE
  ))
})
