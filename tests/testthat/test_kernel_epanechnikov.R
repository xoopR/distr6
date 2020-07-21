library(testthat)

test_that("autotest", {
  autotest_kernel(Epanechnikov,
    shortname = "Epan",
    variance = 1 / 5,
    pdfSquared2Norm = c(3 / 5, 0.20625, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.742, 0.75, 0.742),
    cdf = c(0.4252, 0.5, 0.5748)
  )
})

test_that("cpp", {
  expect_equal(C_EpanechnikovKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
})
