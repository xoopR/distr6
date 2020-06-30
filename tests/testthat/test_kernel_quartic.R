library(testthat)

test_that("autotest", {
  autotest_kernel(Quartic,
    shortname = "Quart",
    variance = 1 / 7,
    pdfSquared2Norm = c(5 / 7, 0.1436942, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.9188, 15 / 16, 0.9188),
    cdf = c(0.4069, 0.5, 0.5931)
  )
})

test_that("cpp", {
  expect_equal(C_QuarticKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
})
