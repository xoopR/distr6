library(testthat)

test_that("autotest", {
  autotest_kernel(UniformKernel,
    shortname = "Unif",
    variance = 1 / 3,
    pdfSquared2Norm = c(1 / 2, 0.25, 0),
    support = Interval$new(-1, 1),
    pdf = rep(0.5, 3),
    cdf = c(0.45, 0.5, 0.55)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- UniformKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- UniformKernel$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.5)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.2)
})

test_that("cdfsquared2norm upper", {
  kern <- UniformKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- UniformKernel$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.02604167)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.14583333)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.00000000)
})
