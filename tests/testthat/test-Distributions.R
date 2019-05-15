library(testthat)

context("Distributions")

test_that("check name validations", {
  expect_silent(Distribution$new("Discrete Test","TestDistr", pdf = dbinom))
  expect_error(Distribution$new(short_name = "Test Distr", pdf = dbinom))
  expect_silent(Distribution$new(short_name = "TestDistr", pdf = dbinom))
  expect_silent(Distribution$new(name = "Test Distr", pdf = dbinom))
})

test_that("check support", {
  expect_equal(Distribution$new("Discrete Test",valueSupport = "c", pdf = dbinom)$valueSupport(), "continuous")
  expect_equal(Distribution$new("Discrete Test",valueSupport = "d", pdf = dbinom)$valueSupport(), "discrete")
  expect_equal(Distribution$new("Discrete Test",valueSupport = "m", pdf = dbinom)$valueSupport(), "mixture")
  expect_error(Distribution$new("Discrete Test",valueSupport = "r", pdf = dbinom))
  expect_equal(Distribution$new("Discrete Test", pdf = dbinom)$valueSupport(), "continuous")
})

test_that("check variate", {
  expect_equal(Distribution$new("Discrete Test",variateForm = "u", pdf = dbinom)$variateForm(), "univariate")
  expect_equal(Distribution$new("Discrete Test",variateForm = "mu", pdf = dbinom)$variateForm(), "multivariate")
  expect_equal(Distribution$new("Discrete Test",variateForm = "ma", pdf = dbinom)$variateForm(), "matrixvariate")
  expect_error(Distribution$new("Discrete Test",variateForm = "m", pdf = dbinom))
  expect_error(Distribution$new("Discrete Test",variateForm = "d", pdf = dbinom))
  expect_equal(Distribution$new("Discrete Test", pdf = dbinom)$variateForm(), "univariate")
  expect_equal(Distribution$new("Discrete Test", pdf = function(x,y) return("Test"), type = Reals$new(2))$variateForm(), "multivariate")
})

test_that("check multivariate", {
  expect_error(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,z) return("Test"),
                                type = Reals$new(2)))
  expect_error(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,y,z) return("Test"),
                                type = Reals$new(2)))
  expect_silent(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                cdf = function(x,y) return("Test"),
                                type = Reals$new(2)))
  expect_error(Distribution$new("Test", pdf = function(x,y) return("Test"),
                                 cdf = function(x,y) return("Test")))
})

dbin = function(x, log,...){
  m1 = choose(self$getParameterValue("size"), x)
  m2 = self$getParameterValue("prob")^x
  m3 = (1-self$getParameterValue("prob"))^(self$getParameterValue("size") - x)
  return(m1 * m2 * m3)
}

ps = ParameterSet$new(id = list("prob","size","qprob"), value = list(0.2, 100, 0.8),
                      lower = list(0, 1, 0), upper = list(1, Inf, 1),
                      class = list("numeric","integer","numeric"),
                      settable = list(TRUE, TRUE, FALSE), fittable = list(TRUE, FALSE, FALSE),
                      updateFunc = list(NULL, NULL, "1 - self$getParameterValue('prob')"),
                      description = list("Probability of Success", "Number of trials",
                                         "Probability of failure"))

test_that("check r/d/p/q", {
  expect_silent(Distribution$new("Test", pdf = dbin, parameters = ps)$pdf(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$cdf(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$quantile(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$rand(1))
})

