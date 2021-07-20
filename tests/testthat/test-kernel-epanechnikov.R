test_that("autotest", {
  autotest_kernel(Epanechnikov,
    shortname = "Epan",
    variance = 1 / 5,
    pdfSquared2Norm = c(3 / 5, 0.20625, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.7425, 0.75, 0.7425),
    cdf = c(0.4252, 0.5, 0.5748)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- Epanechnikov$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Epanechnikov$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.6)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.121344)

})

test_that("cdfsquared2norm upper", {
  kern <- Epanechnikov$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- Epanechnikov$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.0110805)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.1249442)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.0000000)
})
