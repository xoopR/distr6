library(testthat)

context("ParameterSet")

test_that("initialize",{
  expect_error(as.ParameterSet(list()))
  expect_silent(as.ParameterSet(list(id = "Test", value = 2, support = list(Set$new(1:5)), settable = F)))
  expect_silent(as.ParameterSet(data.table::data.table(id = "Test", value = 2, support = list(Set$new(1:5)),
                                           settable = F,
                                     stringsAsFactors = F)))
})


test_that("getters",{
  expect_silent(Binomial$new()$getParameterValue("prob"))
  expect_silent(Binomial$new()$parameters("prob"))
  expect_silent(Binomial$new()$parameters("prodsdsb"))
  expect_silent(Binomial$new()$parameters())
  expect_warning(Binomial$new()$getParameterValue("prob2"))
  expect_silent(Binomial$new()$parameters())
})

test_that("setters",{
  expect_silent(Binomial$new()$setParameterValue(list(size = 5.1)))
  expect_error(Binomial$new()$setParameterValue(list(prob = 2)))
  expect_silent(Binomial$new()$setParameterValue(list(prob = 0.6)))
})

test_that("rbind",{
  expect_error(rbind(Binomial$new()$parameters(),Binomial$new()$parameters()))
  expect_silent(Binomial$new()$parameters()$rbind(Exponential$new(rate=1)$parameters()))
})

test_that("no parameters",{
  expect_null(UniformKernel$new()$parameters())
 UniformKernel$new()$getParameterValue("id")
})
