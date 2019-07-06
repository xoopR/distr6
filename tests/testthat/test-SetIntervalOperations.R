library(testthat)

context("Set interval helpers")

si1 = SetInterval$new(lower = 1, upper = 10, type = "()", dimension = 1, symbol = "S1")
si2 = SetInterval$new(lower = 1, upper = 10, type = "()", dimension = 1, symbol = "S2")

test_that("operation",{
  expect_silent(setOperation("T", si1, si2))
})

test_that("product",{
  expect_silent(product.SetInterval(si1, si2))
  expect_silent(si1 * si2)
  expect_equal(product.SetInterval(si1, si2), si1 * si2)
})

test_that("union",{
  expect_silent(union.SetInterval(si1, si2))
  expect_silent(si1 + si2)
  expect_equal(union.SetInterval(si1, si2), si1 + si2)
  expect_equal(union.SetInterval(si1, si1), si1)
})

test_that("power",{
  expect_silent(power.SetInterval(si1, 2))
  expect_silent(si1 ^ 2)
  expect_equal(power.SetInterval(si1, 2), si1 ^ 2)
})

test_that("complement",{
  x = Interval$new(4,8)
  y = Interval$new(2,10)
  expect_equal((x-y)$getSymbol(), Empty$new()$getSymbol())
  y = Interval$new(4,8)
  expect_equal((x-y)$getSymbol(), Empty$new()$getSymbol())

  y = Interval$new(1,3)
  expect_equal((x-y)$getSymbol(), x$getSymbol())
  y = Interval$new(9,100)
  expect_equal((x-y)$getSymbol(), x$getSymbol())

  y = Interval$new(4,4)
  expect_equal((x-y)$getSymbol(), Interval$new(4,8,type="(]")$getSymbol())
  y = Interval$new(8,8)
  expect_equal((x-y)$getSymbol(), Interval$new(4,8,type="[)")$getSymbol())

  y = Interval$new(0,6)
  expect_equal((x-y)$getSymbol(), Interval$new(6,8,type="(]")$getSymbol())

  y = Interval$new(7,10)
  expect_equal((x-y)$getSymbol(), Interval$new(4,7,type="[)")$getSymbol())

  y = Interval$new(5,7)
  expect_equal((x-y)$getSymbol(), union.SetInterval(Interval$new(4,5,type="[)"),
                                        Interval$new(7,8,type="(]"))$getSymbol())

  y = Set$new(5)
  expect_equal((x-y)$getSymbol(), union.SetInterval(Interval$new(4,5,type="[)"),
                                                    Interval$new(5,8,type="(]"))$getSymbol())
})

test_that("setSymbol",{
  expect_silent(setSymbol("Reals"))
  expect_silent(setSymbol(Reals))
  expect_silent(setSymbol(posintegers))
  expect_silent(setSymbol(PosIntegers))
})
