library(testthat)

test_that("autotest", {
  autotest_kernel(Triweight,
    shortname = "Triw",
    variance = 1 / 9,
    pdfSquared2Norm = c(350 / 429, 0.09383479, 0),
    support = Interval$new(-1, 1),
    pdf = c(1.0613, 1.0938, 1.0613),
    cdf = c(0.3917, 0.5, 0.6083)
  )
})

test_that("cpp", {
  expect_equal(C_TriweightKernelCdf(c(2, -2), TRUE, FALSE), c(1, 0))
})
