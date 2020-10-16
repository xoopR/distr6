test_that("autotest", {
  autotest_kernel(TriangularKernel,
    shortname = "Tri",
    variance = 1 / 6,
    pdfSquared2Norm = c(2 / 3, 1 / 6, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.9, 1, 0.9),
    cdf = c(0.405, 0.5, 0.595)
  )
})

test_that("pdfsquared2norm upper", {
  kern <- TriangularKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- TriangularKernel$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.66666667)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.08533333)
})

test_that("cdfsquared2norm upper", {
  kern <- TriangularKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- TriangularKernel$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.008072917)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.117187500)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.000000000)
})
