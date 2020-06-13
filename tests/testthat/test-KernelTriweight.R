library(testthat)

test_that("autotest", {
  autotest_kernel(Triweight,
    shortname = "Triw",
    variance = 1 / 9,
    pdfSquared2Norm = 350 / 429,
    support = Interval$new(-1, 1),
    pdf = c(1.0613, 1.0938, 1.0613),
    cdf = c(0.3917, 0.5, 0.6083)
  )
})
