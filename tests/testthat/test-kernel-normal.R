test_that("autotest", {
  autotest_kernel(NormalKernel,
    shortname = "Norm",
    variance = 1,
    pdfSquared2Norm = c(1 / (2 * sqrt(pi)), 0.21969560, 0.02973257),
    support = Reals$new(),
    pdf = c(0.3970, 0.3989, 0.3970),
    cdf = c(0.4602, 0.5, 0.5398)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- NormalKernel$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- NormalKernel$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0.1037769)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.2820948)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.1968109)
})
