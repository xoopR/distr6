library(testthat)

context("Set interval helpers")

si1 = SetInterval$new(lower = 1, upper = 10, type = "()", dimension = 1, symbol = "S1")
si2 = SetInterval$new(lower = 1, upper = 10, type = "()", dimension = 1, symbol = "S2")

test_that("operation",{
  expect_silent(operation("T", si1, si2))
})

test_that("product",{
  expect_silent(product(si1, si2))
  expect_silent(si1 * si2)
  expect_equal(product(si1, si2), si1 * si2)
})

test_that("union",{
  expect_silent(union(si1, si2))
  expect_silent(si1 + si2)
  expect_equal(union(si1, si2), si1 + si2)
})

test_that("power",{
  expect_silent(power(si1, 2))
  expect_silent(si1 ^ 2)
  expect_equal(power(si1, 2), si1 ^ 2)
})

test_that("setSymbol",{
  expect_silent(setSymbol("Reals"))
  expect_silent(setSymbol(Reals))
  expect_silent(setSymbol(posintegers))
  expect_silent(setSymbol(PosIntegers))
})
