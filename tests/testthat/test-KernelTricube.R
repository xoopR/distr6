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


test_that("pdfsquared2norm upper", {
  kern <- Tricube$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Tricube$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.70850202)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.06266124)
})
