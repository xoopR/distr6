library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Degenerate,
    pars = list(mean = 1),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Set$new(1, class = "numeric"),
    symmetry = "symmetric",
    mean = 1,
    mode = 1,
    median = 1,
    variance = 0,
    skewness = 0,
    exkur = NaN,
    entropy = 0,
    mgf = exp(1),
    cf = exp(1i),
    pgf = NaN,
    pdf = c(1, 0, 0),
    cdf = c(1, 1, 1),
    quantile = c(1, 1, 1)
  )
})

test_that("manual", {
  dist <- Degenerate$new()
  expect_equal(dist$quantile(0), -Inf)
})

test_that("cpp", {
  expect_equal(as.numeric(C_DegenerateQuantile(-2, 0, TRUE, FALSE)), NaN)
  expect_equal(as.numeric(C_DegenerateQuantile(0, 0, TRUE, FALSE)), -Inf)
})
