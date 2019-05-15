library(testthat)

context("Set Interval")

test_that("initialize",{
  expect_silent(SetInterval$new(symbol = "T",upper = 10, lower = 1, dimension = 2, type = "{}"))
  expect_error(SetInterval$new(upper = 10, lower = 1, dimension = 2, type = "{}"))
  expect_error(SetInterval$new(symbol = "T",lower = 1, dimension = 2, type = "{}"))
  expect_error(SetInterval$new(symbol = "T",upper = 10, dimension = 2, type = "{}"))
  expect_error(SetInterval$new(symbol = "T",upper = 10, lower = 1, type = "{}"))
  expect_error(SetInterval$new(symbol = "T",upper = 10, lower = 1, dimension = 2))
})

si = SetInterval$new(symbol = "T",upper=10,lower=1,dimension = 1,type="[]")
test_that("getters",{
  expect_equal(si$max(), 10)
  expect_equal(si$min(), 1)
  expect_equal(si$sup(), 10)
  expect_equal(si$inf(), 1)
  expect_equal(si$dimension(), 1)
  expect_equal(si$type(), "[]")
  expect_equal(si$getSymbol(), "T")
  expect_output(si$print())
})

si = SetInterval$new(symbol = "T",upper=10,lower=1,dimension = 1,type="()")
test_that("getters ()",{
  expect_equal(si$max(), 10)
  expect_equal(si$min(), 1)
  expect_equal(si$sup(), 10 - .Machine$double.eps)
  expect_equal(si$inf(), 1 + .Machine$double.eps)
  expect_equal(si$dimension(), 1)
  expect_equal(si$type(), "()")
  expect_equal(si$getSymbol(), "T")
})
