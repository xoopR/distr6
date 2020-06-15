library(testthat)

test_that("autotest", {
  autotest_kernel(NormalKernel,
    shortname = "Norm",
    variance = 1,
    pdfSquared2Norm = c(1 / (2 * sqrt(pi)), 01, 03),
    support = Reals$new(),
    pdf = c(0.3970, 0.3989, 0.3970),
    cdf = c(0.4602, 0.5, 0.5398)
  )
})
