library(testthat)

test_that("autotest", {
  autotest_kernel(LogisticKernel,
    shortname = "Logis",
    variance = (pi^2) / 3,
    pdfSquared2Norm = 1 / 6,
    support = Reals$new(),
    pdf = c(0.2494, 0.2500, 0.2494),
    cdf = c(0.4750, 0.5, 0.5250)
  )
})
