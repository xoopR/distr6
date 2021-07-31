test_that("autotest", {
  autotest_kernel(Sigmoid,
    shortname = "Sigm",
    variance = (pi^2) / 4,
    pdfSquared2Norm = c(2 / (pi^2), 0.17243210, 0.06068424),
    support = Reals$new(),
    pdf = c(0.3167, 0.3183, 0.3167)
  )
})


test_that("pdfsquared2norm upper", {
  kern <- Sigmoid$new(decorators = "ExoticStatistics")
  expect_rounded_equal(kern$pdfPNorm(2, upper = 2)^2, kern$pdfSquared2Norm(upper = 2), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = 0)^2, kern$pdfSquared2Norm(upper = 0), 4)
  expect_rounded_equal(kern$pdfPNorm(2, upper = -1)^2, kern$pdfSquared2Norm(upper = -1), 4)
})

test_that("pdfsquared2norm x", {
  kern <- Sigmoid$new()
  expect_rounded_equal(kern$pdfSquared2Norm(x = 2), 0.1117453)
  expect_rounded_equal(kern$pdfSquared2Norm(x = 0), 0.2026424)
  expect_rounded_equal(kern$pdfSquared2Norm(x = -1.2), 0.1610978)
})
