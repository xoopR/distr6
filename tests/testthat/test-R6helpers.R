library(testthat)

context("R6 helpers")

test_that("strprint",{
  expect_equal(strprint.Distribution(Exponential$new()),"Exp(rate = 1)")
  expect_error(strprint.list(list(Exponential$new(),Exponential$new())),list("Exponential","Exponential"))
})

test_that("getR6class",{
  expect_silent(getR6Class(Binomial$new()))
})