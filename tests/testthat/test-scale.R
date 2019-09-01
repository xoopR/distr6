# library(testthat)
#
# context("Scale Wrapper")
#
# test_that("check continuous scale wrapper", {
#   scaleExp = Scale$new(Exponential$new())
#   expect_message(decorate(scaleExp, CoreStatistics))
#   expect_equal(round(scaleExp$genExp(), 5), 0)
#   expect_equal(round(scaleExp$variance(), 5), 1)
# })
#
# test_that("check discrete scale wrapper", {
#   discreteTesterScaled = Scale$new(discreteTester)
#   expect_equal(round(discreteTesterScaled$expectation(), 5), 0)
#   expect_equal(round(discreteTesterScaled$variance(), 5), 1)
# })
