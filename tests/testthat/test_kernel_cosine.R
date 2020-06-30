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

test_that("cpp", {
  expect_equal(C_CosineKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
  expect_equal(C_CosineKernelQuantile(c(2, 0, 1), TRUE, FALSE), c(NaN, -1, 1))
})
