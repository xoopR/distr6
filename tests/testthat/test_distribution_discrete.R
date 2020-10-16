# library(testthat)
#
# context("Custom discrete distributions")
#
# dbin <- function(x) {
#   m1 <- choose(self$getParameterValue(id = "size"), x)
#   m2 <- self$getParameterValue(id = "prob")^x
#   m3 <- (1 - self$getParameterValue(id = "prob"))^(self$getParameterValue(id = "size") - x)
#   return(m1 * m2 * m3)
# }
#
# ps <- ParameterSet$new(
#   id = list("prob", "size", "qprob"), value = list(0.2, 100, 0.8),
#   support = list(Interval$new(0, 1), PosNaturals$new(), Interval$new(0, 1)),
#   settable = list(TRUE, TRUE, FALSE),
#   description = list(
#     "Probability of Success", "Number of trials",
#     "Probability of failure"
#   )
# )
#
# discreteTester <- Distribution$new("Discrete Test", "TestDistr",
#   support = Set$new(0:10, class = "integer"),
#   symmetric = TRUE, type = PosNaturals$new(),
#   pdf = dbin,
#   parameters = ps,
#   decorators = list(CoreStatistics)
# )
#
# test_that("self in arg", {
#   dbin <- function(x, self) {
#     m1 <- choose(self$getParameterValue(id = "size"), x)
#     m2 <- self$getParameterValue(id = "prob")^x
#     m3 <- (1 - self$getParameterValue(id = "prob"))^(self$getParameterValue(id = "size") - x)
#     return(m1 * m2 * m3)
#   }
#
#   pbin <- function(x, self) {
#     m1 <- choose(self$getParameterValue(id = "size"), x)
#     m2 <- self$getParameterValue(id = "prob")^x
#     m3 <- (1 - self$getParameterValue(id = "prob"))^(self$getParameterValue(id = "size") - x)
#     return(m1 * m2 * m3)
#   }
#
#   rbin <- function(n, self) {
#     return(n)
#   }
#
#   expect_silent(Distribution$new("Discrete Test", "TestDistr",
#     support = Set$new(0:10),
#     symmetric = TRUE, type = PosNaturals$new(),
#     pdf = dbin, cdf = pbin, rand = rbin,
#     parameters = ps
#   ))
# })
#
# test_that("check all accessors are working", {
#   expect_equal(discreteTester$strprint(), "TestDistr(prob = 0.2, size = 100)")
#   expect_equal(discreteTester$name, "Discrete Test")
#   expect_equal(discreteTester$short_name, "TestDistr")
#   expect_equal(discreteTester$description, NULL)
#   expect_equal(discreteTester$decorators, "CoreStatistics")
#   expect_equal(discreteTester$valueSupport, "discrete")
#   expect_equal(discreteTester$variateForm, "univariate")
#   expect_equal(discreteTester$symmetry, "symmetric")
#   expect_equal(discreteTester$getParameterValue("size"), 100)
# })
#
# test_that("check parameter getting/setting", {
#   expect_warning(discreteTester$setParameterValue("sgdsvfd"))
#   expect_silent(discreteTester$setParameterValue(size = 2, prob = 0.9))
#   expect_silent(discreteTester$setParameterValue(lst = list(size = 2, prob = 0.9)))
#   expect_equal(discreteTester$getParameterValue("prob"), 0.9)
# })
#
# test_that("check basic maths functions as expected", {
#   expect_equal(discreteTester$pdf(1), dbinom(1, 2, 0.9))
#   expect_equal(discreteTester$genExp(), 2 * 0.9)
#   expect_equal(discreteTester$variance(), 2 * 0.9 * 0.1)
#   expect_null(discreteTester$median())
# })
#
# test_that("check kurtosis and skewness", {
#   expect_equal(round(discreteTester$kurtosis(), 2), 2.56)
#   expect_equal(discreteTester$kurtosisType, "leptokurtic")
#   expect_equal(round(discreteTester$skewness(), 2), -1.89)
#   expect_equal(discreteTester$skewnessType, "negative skew")
# })
#
# test_that("check exotic functions silent", {
#   expect_silent(discreteTester$mode())
#   expect_message(discreteTester$kthmoment(2))
#   expect_message(discreteTester$kthmoment(3, type = "standard"))
#   expect_silent(discreteTester$pgf(z = 2))
#   expect_message(discreteTester$entropy())
# })
#
# test_that("check mgf, cf, pgf", {
#   expect_equal(discreteTester$mgf(4), (1 - 0.9 + 0.9 * exp(4))^2)
#   expect_equal(discreteTester$cf(4), (1 - 0.9 + 0.9 * exp(4i))^2)
#   expect_equal(discreteTester$pgf(2), (1 - 0.9 + 0.9 * 2)^2)
# })
#
# test_that("representations", {
#   expect_output(discreteTester$print())
#   expect_silent(discreteTester$strprint())
#   expect_output(discreteTester$summary(T))
#   expect_output(discreteTester$summary(F))
# })
