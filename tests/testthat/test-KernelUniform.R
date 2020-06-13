library(testthat)

test_that("autotest", {
  autotest_kernel(UniformKernel,
    shortname = "Unif",
    variance = 1 / 3,
    pdfSquared2Norm = 1 / 2,
    support = Interval$new(-1, 1),
    pdf = rep(0.5, 3),
    cdf = c(0.45, 0.5, 0.55)
  )
})
