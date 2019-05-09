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
