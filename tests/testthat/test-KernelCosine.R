library(testthat)

test_that("autotest", {
  autotest_kernel(Cosine,
    shortname = "Cos",
    variance = 1 - (8 / (pi^2)),
    pdfSquared2Norm = c((pi^2) / 16, 0.1963495, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.7757, 0.7854, 0.7757),
    cdf = c(0.4218, 0.5, 0.5782)
  )
})

test_that("pdfsquared2norm upper", {
  kern <- Cosine$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Cosine$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.6168503)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.1104926)
})


test_that("cdfsquared2norm upper", {
  kern <- Cosine$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- Cosine$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.01003923)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.122963637)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.0000000)
})
