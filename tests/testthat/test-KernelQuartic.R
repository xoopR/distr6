library(testthat)

test_that("autotest", {
  autotest_kernel(Quartic,
    shortname = "Quart",
    variance = 1 / 7,
    pdfSquared2Norm = 5 / 7,
    support = Interval$new(-1, 1),
    pdf = c(0.9188, 15 / 16, 0.9188),
    cdf = c(0.4069, 0.5, 0.5931)
  )
})
