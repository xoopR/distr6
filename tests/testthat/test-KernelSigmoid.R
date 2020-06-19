library(testthat)

test_that("autotest", {
  autotest_kernel(Sigmoid,
    shortname = "Sigm",
    variance = (pi^2) / 4,
    pdfSquared2Norm = c(2 / (pi^2), 0.17243210, 0.06068424),
    support = Reals$new(),
    pdf = c(0.3167, 0.3183, 0.3167)
  )
})
