test_that("autotest", {
  autotest_kernel(LogisticKernel,
    shortname = "Logis",
    variance = (pi^2) / 3,
    pdfSquared2Norm = c(1 / 6, 0.1509476, 0.0724759),
    support = Reals$new(),
    pdf = c(0.2494, 0.2500, 0.2494),
    cdf = c(0.4750, 0.5, 0.5250)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- LogisticKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- LogisticKernel$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0.1133284)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.1666667)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.1445932)
})


test_that("cdfsquared2norm upper", {
  kern <- LogisticKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- LogisticKernel$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.136382)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.260096)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.0239595)
})
