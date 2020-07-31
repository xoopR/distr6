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
  expect_equal(Distribution$new(short_name = "TestDistr", pdf = dbin, type = Naturals$new(),
                                variateForm = "multi")$traits$variateForm, "multivariate")
  expect_equal(Distribution$new(short_name = "TestDistr", pdf = dbin,
                                type = Naturals$new()^2)$traits$variateForm, "multivariate")
})


test_that("check support", {
  expect_equal(Distribution$new("Discrete Test", valueSupport = "c", pdf = dbin,
                                type = Naturals$new())$traits$valueSupport, "continuous")
  expect_equal(Distribution$new("Discrete Test", valueSupport = "d", pdf = dbin,
                                type = Naturals$new())$traits$valueSupport, "discrete")
  expect_equal(Distribution$new("Discrete Test", valueSupport = "m", pdf = dbin,
                                type = Naturals$new())$traits$valueSupport, "mixture")
  expect_error(Distribution$new("Discrete Test", valueSupport = "r", pdf = dbin,
                                type = Naturals$new()))
  expect_equal(Distribution$new("Discrete Test", pdf = dbin,
                                type = Naturals$new())$traits$valueSupport,
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
ps$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
ps$addDeps("qprob", "prob", function(self) 1 - self$getParameterValue("qprob"))

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

test_that("log", {
  expect_error(Distribution$new(name = "Test Distr", pdf = function(x) x, type = Naturals$new())$
                 pdf(1, log = TRUE),
               "No analytical")
  expect_error(Distribution$new(name = "Test Distr", cdf = function(x) x, type = Naturals$new())$
                 cdf(1, log.p = TRUE),
               "No analytical")
  expect_error(Distribution$new(name = "Test Distr", cdf = function(x) x,
                                quantile = function(p) p, type = Naturals$new())$
                 quantile(log(1), log.p = TRUE),
               "No analytical")
  expect_equal(Distribution$new(name = "Test Distr", cdf = function(x) x,
                                quantile = function(p) p, type = Naturals$new(),
                                decorators = "CoreStatistics")$
                 quantile(log(1), log.p = TRUE, lower.tail = FALSE),
               0)
})

test_that("working_support", {
  expect_equal(Exponential$new()$workingSupport(), Interval$new(0, 100))
  expect_equal(Binomial$new()$workingSupport(), Set$new(elements = 0:10, class = "integer"))
  expect_equal(Normal$new()$workingSupport(), Interval$new(-100, 10))
  expect_equal(Distribution$new("Test", pdf = dbin, parameters = ps,
                                type = Integers$new())$workingSupport(),
               Interval$new(-10, 1000, class = "integer"))
})

test_that("print/summary", {
  expect_output(Binomial$new()$print(1))
  expect_output(Binomial$new()$print(5))
  expect_output(Distribution$new(name = "Test Distr", pdf = dbin, type = Naturals$new(),
                                 decorators = "CoreStatistics")$summary(), "Decorated with")
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
    .suppressChecks = TRUE
  ))
})

test_that("median", {
  d <- Distribution$new(name = "a", pdf = function(x) x, type = Reals$new(),
                        quantile = function(p) p, symmetric = TRUE)
  expect_equal(d$median(), 0.5)
  d$mean <- function() NULL
  expect_equal(d$median(), 0.5)
  d$mean <- function() 1
  expect_equal(d$median(), 1)
})

test_that("iqr", {
  expect_equal(Binomial$new()$iqr(), Binomial$new()$quantile(0.75) - Binomial$new()$quantile(0.25))
})

test_that("correlation", {
  expect_equal(Binomial$new()$correlation(), 1)
  mn <- Multinomial$new()
  expect_equal(mn$correlation(),
               mn$variance() / (sqrt(diag(mn$variance()) %*% t(diag(mn$variance())))))
})

test_that("deprecated", {
  b <- Binomial$new()
  expect_message(b$type, "Deprecated")
  expect_message(b$variateForm, "Deprecated")
  expect_message(b$valueSupport, "Deprecated")
  expect_message(b$kurtosisType, "Deprecated")
  expect_message(b$skewnessType, "Deprecated")
  expect_message(b$support, "Deprecated")
  expect_message(b$symmetry, "Deprecated")
})

test_that("no dpqr given", {
  b <- Binomial$new()
  expect_null(b$pdf())
  expect_null(b$cdf())
  expect_null(b$quantile())
  expect_null(b$rand())
})

test_that("points outside domain", {
  expect_error(Multinomial$new()$pdf(data = matrix(c(-1, 12, 2, 8), ncol = 2)), "Not all points")
  expect_error(EmpiricalMV$new()$cdf(data = matrix(c(-1i, 12, 2, 8), ncol = 2)), "Not all points")

  expect_error(Binomial$new()$pdf(-1), "Not all points")
  expect_error(Binomial$new()$cdf(-1), "Not all points")
  expect_error(Binomial$new()$quantile(-1), "Not all points")
  expect_error(Binomial$new()$quantile(2, log.p = TRUE), "Not all points")
})
