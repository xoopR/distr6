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

test_that("check r/d/p/q", {
  expect_error(Distribution$new("Test", pdf = dbinom)$pdf(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$cdf(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$quantile(1))
  expect_null(Distribution$new("Test", pdf = dbinom)$rand(1))
})

