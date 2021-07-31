test_that("autotest", {
  autotest_kernel(Tricube,
    shortname = "Tric",
    variance = 35 / 243,
    pdfSquared2Norm = c(175 / 247, 0.1514387, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.8616, 0.8642, 0.8616),
    cdf = c(0.4137, 0.5000, 0.58636)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- Tricube$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Tricube$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.70850202)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.06266124)
})

# test_that("cdfsquared2norm upper", {
#   kern <- Tricube$new(decorators = "ExoticStatistics")
#   expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 2)
#   expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 2)
#   expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
# })

test_that("cdfsquared2norm x", {
  kern <- Tricube$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.005515562)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.005515562)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.000000000)
})
