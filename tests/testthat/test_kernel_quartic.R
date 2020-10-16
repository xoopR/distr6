test_that("autotest", {
  autotest_kernel(Quartic,
    shortname = "Quart",
    variance = 1 / 7,
    pdfSquared2Norm = c(5 / 7, 0.1436942, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.9188, 15 / 16, 0.9188),
    cdf = c(0.4069, 0.5, 0.5931)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- Quartic$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Quartic$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.71428571)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.06180279)
})

test_that("cdfsquared2norm upper", {
  kern <- Quartic$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- Quartic$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.005709188)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.114143405)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.000000000)
})
