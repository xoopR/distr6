library(testthat)

test_that("numeric_cdf_discrete", {
  expect_equal(C_NumericCdf_Discrete(-2, 1, 1, TRUE, FALSE), 0)
  expect_equal(C_NumericCdf_Discrete(-2, 1, 1, FALSE, TRUE), 0)
})

test_that("numeric_quantile", {
  expect_error(C_NumericQuantile(1, 1, 1, FALSE, TRUE), "All values")
  expect_equal(C_NumericQuantile(c(0, 1), 1:3, (1:3) / 3, TRUE, FALSE),
               c(1, 3))
})
