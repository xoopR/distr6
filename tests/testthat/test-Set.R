library(testthat)

context("Set")

test_that("initialize",{
  expect_silent(Set$new())
  expect_silent(Set$new("a","b","c"))
  expect_silent(Set$new(1,5,7))
  expect_silent(Set$new(5))
})


test_that("elements",{
  s = Set$new("a","b","c")
  expect_equal(s$elements(),c("a","b","c"))
  expect_equal(s$length(),3)
  expect_true(s$liesInSetInterval("a"))
  expect_false(s$liesInSetInterval("d"))
})
