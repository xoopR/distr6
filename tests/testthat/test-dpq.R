library(testthat)

test_that("catch error", {
  expect_error(C_dpq("rnorm", 1, list(sd = 1, mean = 2)), "Function must start")
})
