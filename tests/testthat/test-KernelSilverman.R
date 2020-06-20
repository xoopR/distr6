library(testthat)

test_that("autotest", {
  autotest_kernel(Silverman,
    shortname = "Silv",
    variance = 0,
    pdfSquared2Norm = c((3 * sqrt(2)) / 16, 0.2243738, 0.04877213),
    support = Reals$new(),
    pdf = c(0.3519, 0.3536, 0.3519)
  )
})
