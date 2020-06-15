library(testthat)

test_that("autotest", {
  autotest_kernel(Tricube,
    shortname = "Tric",
    variance = 35 / 243,
    pdfSquared2Norm = c(175 / 247, 01, 03),
    support = Interval$new(-1, 1),
    pdf = c(0.8616, 0.8642, 0.8616)
  )
})
