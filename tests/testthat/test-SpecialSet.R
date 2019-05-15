library(testthat)

context("Special sets")

test_that("test abstract",{
  expect_error(SpecialSet$new())
})

test_that("special constructors",{
  expect_silent(Complex$new())
  expect_silent(Integers$new())
  expect_silent(Naturals$new())
  expect_silent(NegIntegers$new())
  expect_silent(PosIntegers$new())
  expect_silent(PosNaturals$new())
  expect_silent(Rationals$new())
  expect_silent(Reals$new())
  expect_silent(ExtendedReals$new())
  expect_silent(NegRationals$new())
  expect_silent(NegReals$new())
  expect_silent(PosRationals$new())
  expect_silent(PosReals$new())
})
