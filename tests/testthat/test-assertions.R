library(testthat)

context("is/check/assert/testThat")

test_that("isThat",{
  expect_true(isThat(1 == 1))
  expect_false(isThat(1 == 2))
})

test_that("testThat",{
  expect_true(testThat(1 == 1))
  expect_false(testThat(1 == 2))
})

test_that("checkThat",{
  expect_true(checkThat(1 == 1,"error"))
  expect_equal(checkThat(1 == 2,"error"),"error")
})

test_that("assertThat",{
  expect_equal(assertThat(3, 1 == 1,"error"),3)
  expect_error(assertThat(1 == 2,"error"))
})

test_that("makeChecks",{
  expect_error(makeChecks("test",1==1,"error"))
})
