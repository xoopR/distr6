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


test_that("pdfsquared2norm upper", {
  kern <- Triweight$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Triweight$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.81585082)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.02944901)
})

test_that("cdfsquared2norm upper", {
  kern <- Triweight$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$cdfPNorm(2, upper = 2)^2, kern$cdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = 0)^2, kern$cdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$cdfPNorm(2, upper = -1)^2, kern$cdfSquared2Norm(upper = -1), 4)
})

test_that("cdfsquared2norm x", {
  kern <- Triweight$new()
  expect_rounded_equal(kern$cdfSquared2Norm(x = 0.5), 0.003222043)
  expect_rounded_equal(kern$cdfSquared2Norm(x = -0.5), 0.106718607)
  expect_rounded_equal(kern$cdfSquared2Norm(x = 2.5), 0.0000000)
})
