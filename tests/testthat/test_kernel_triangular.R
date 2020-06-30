library(testthat)

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

test_that("cpp", {
  expect_equal(C_TriangularKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
  expect_equal(C_TriangularKernelQuantile(c(2, 0, 1, 0.5), TRUE, FALSE), c(NaN, -Inf, Inf, 0))
})
