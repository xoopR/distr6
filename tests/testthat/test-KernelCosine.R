library(testthat)

test_that("autotest", {
  autotest_kernel(Cosine,
    shortname = "Cos",
    variance = 1 - (8 / (pi^2)),
    pdfSquared2Norm = (pi^2) / 16,
    support = Interval$new(-1, 1),
    pdf = c(0.7757, 0.7854, 0.7757),
    cdf = c(0.4218, 0.5, 0.5782)
  )
})
