library(testthat)

context("RSmisc")

test_that("assertThat",{
  expect_silent(assertThat(Binomial$new(),Binomial$new()$short_name=="Binom","Not True"))
  expect_error(assertThat(Binomial$new(),Binomial$new()$short_name=="Dinom","Not True"))
})

test_that("checkThat",{
  expect_true(checkThat(Binomial$new()$short_name=="Binom","Not True"))
  expect_equal(checkThat(Binomial$new()$short_name=="Dinom","Not True"),"Not True")
})

test_that("testThat",{
  expect_true(testThat(Binomial$new()$short_name=="Binom"))
  expect_false(testThat(Binomial$new()$short_name=="Dinom"))
})

test_that("isThat",{
  expect_true(isThat(Binomial$new()$short_name=="Binom"))
  expect_false(isThat(Binomial$new()$short_name=="Dinom"))
})

test_that("makeChecks",{
  expect_silent(makeChecks("Test", 1==1, "Error"))
})

test_that("getR6Class",{
  expect_equal(getR6Class(Binomial$new()),"Binomial")
  expect_equal(getR6Class(Binomial$new(), classname = F),Binomial)
})

test_that("stopwarn",{
  expect_warning(expect_null(stopwarn(error = "warn", "Warning")))
  expect_error(stopwarn(error = "stop", "Warning"))
})

test_that("testmessage",{
  expect_true(testMessage(message("Hi")))
  expect_warning(expect_false(testMessage(warning("Hi"))))
})

test_that("ifnerror",{
  expect_equal(ifnerror(stop("Error"),"Success","Failure",silent = T),"Failure")
  expect_equal(ifnerror("Nerror","Success","Failure",silent = T),"Success")
  expect_warning(ifnerror(stop("Error"),"Success","warn",silent = T))
  expect_error(ifnerror(stop("Error"),"Success","stop",silent = T))
})

test_that("modal",{
  expect_equal(modal(c(1,2,2,4,5,6,7,2,4,4,2,4,2)), 2)
  expect_equal(modal(c(1,2,2,4,5,6,7,2,4,4,2,4,2,4)), c(2,4))
})

test_that("toproper",{
  expect_equal(toproper("a long SenTENCe"), "A Long Sentence")
  expect_equal(toproper("DifFERent-spLIT", split = "-"), "Different-Split")
})
