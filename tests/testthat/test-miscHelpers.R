library(testthat)

context("Misc helpers")

msgFun <- function() message("Test Function")
noMsgFun <- function() warning("Test Function")

test_that("test message function", {
  expect_true(testMessage(msgFun()))
  expect_false(testMessage(noMsgFun()))
})
