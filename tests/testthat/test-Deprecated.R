library(testthat)

context("Deprecated")

test_that("Derprecated",{
  expect_warning(ArrayDistribution$new(), "deprecated")
  expect_warning(listSpecialSets(), "deprecated")
})
