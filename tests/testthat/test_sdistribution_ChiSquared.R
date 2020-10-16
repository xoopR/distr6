library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = ChiSquared,
    pars = list(df = 8),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 8,
    mode = 6,
    median = 7.3441,
    variance = 16,
    skewness = 1,
    exkur = 1.5,
    entropy = 3.81661,
    mgf = NaN,
    cf = -0.0112 - 0.0384i,
    pgf = 1,
    pdf = dchisq(1:3, 8),
    cdf = pchisq(1:3, 8),
    quantile = qchisq(c(0.24, 0.42, 0.5), 8)
  )
})

test_that("manual", {
  dist <- ChiSquared$new(df = 1)
  expect_equal(dist$mgf(0.1), (0.8)^-0.5)
  expect_equal(dist$pgf(3), NaN)
})
