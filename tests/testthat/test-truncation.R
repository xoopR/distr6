test_that("check continuous Truncated wrapper", {
  TruncExp = TruncatedDistribution$new(Exponential$new(), lower = 1, upper = 5)
  expect_equal(TruncExp$support()$numeric(), 1:5)
  expect_equal(TruncExp$pdf(0), 0)
  expect_equal(TruncExp$pdf(6), 0)
})

test_that("check discrete Truncated wrapper", {
  TruncBin = TruncatedDistribution$new(Binomial$new(), lower = 1, upper = 5)
  expect_equal(TruncBin$support()$numeric(), 1:5)
  expect_equal(TruncBin$pdf(0), 0)
  expect_equal(TruncBin$pdf(6), 0)
})

test_that("check truncation constructor",{
  expect_silent(truncate(Binomial$new(),lower = 1, upper = 5))
  expect_silent(truncate.Distribution(Binomial$new(), upper = 5))
  expect_silent(truncate(Binomial$new(),lower = 1))
  expect_silent(truncate(Binomial$new()))
})

test_that("check cdf",{
  expect_error(truncate(Distribution$new("Test"),1,2))
})
