library(testthat)

context("Set")

test_that("initialize",{
  expect_silent(Set$new())
  expect_silent(Set$new("a","b","c"))
  expect_silent(Set$new(1,5,7))
  expect_silent(Set$new(5))

})
