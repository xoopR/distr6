library(testthat)

test_that("autotest", {
  autotest_kernel(Tricube,
    shortname = "Tric",
    variance = 35 / 243,
    pdfSquared2Norm = c(175 / 247, 0.1514387, 0),
    support = Interval$new(-1, 1),
    pdf = c(0.8616, 0.8642, 0.8616)
  )
})
