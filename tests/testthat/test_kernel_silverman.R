test_that("autotest", {
  autotest_kernel(Silverman,
    shortname = "Silv",
    variance = 0,
    pdfSquared2Norm = c((3 * sqrt(2)) / 16, 0.2243738, 0.04877213),
    support = Reals$new(),
    pdf = c(0.3519, 0.3536, 0.3519),
    cdf = c(0.465, 0.500, 0.535)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- Silverman$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Silverman$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0.1337661)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.2651650)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.2083717)
})


# test_that("cdfsquared2norm upper", {
#   kern <- Silverman$new(decorators = "ExoticStatistics")
#   expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
#   expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
#   expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
# })

test_that("cdfsquared2norm x", {
  kern <- Silverman$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.07659512)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.19904861)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 1.3), 0.01805105)
})
