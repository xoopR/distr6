library(testthat)

test_that("autotest", {
  autotest_kernel(UniformKernel,
    shortname = "Unif",
    variance = 1 / 3,
    pdfSquared2Norm = c(1 / 2, 0.25, 0),
    support = Interval$new(-1, 1),
    pdf = rep(0.5, 3),
    cdf = c(0.45, 0.5, 0.55)
  )
})

test_that("cpp", {
  expect_equal(C_UniformKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
  expect_equal(C_UniformKernelQuantile(c(2, 0, 1, 0.5), TRUE, FALSE), c(NaN, -Inf, Inf, 0))
})
