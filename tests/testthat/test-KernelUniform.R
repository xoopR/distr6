library(testthat)

u = KUniform$new()
test_that("statistics",{
  expect_equal(u$mean(), 0)
  expect_equal(u$var(), 1/3)
  expect_equal(u$squared2Norm(), 0.5)
  expect_equal(u$pdf(1), 0.5)
  expect_equal(u$cdf(0.6), 0.8)
  expect_equal(u$quantile(0.324), -0.352)
  expect_equal(u$cdf(u$quantile(0.324)), 0.324)
  expect_silent(u$rand(10))
})
