library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Triangular,
    pars = list(lower = 0, upper = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    mean = 0.5,
    mode = 0.5,
    median = 0.5,
    variance = 0.75 / 18,
    skewness = 0,
    exkur = -0.6,
    entropy = 0.5 + log(0.5, 2),
    mgf = 2 * (0.5 - exp(0.5) + 0.5 * exp(1)) / (0.25),
    cf = (-2 * (0.5 - exp(0.5i) + 0.5 * exp(1i))) / (0.25),
    pgf = NaN,
    pdf = extraDistr::dtriang(1:3, 0, 1, 0.5),
    cdf = extraDistr::ptriang(1:3, 0, 1, 0.5),
    quantile = extraDistr::qtriang(c(0.24, 0.42, 0.5), 0, 1, 0.5)
  )
})

test_that("manual", {
  expect_true(testSymmetric(Triangular$new(lower = 1, upper = 2, mode = 1.8, symmetric = TRUE)))
  expect_true(testSymmetric(Triangular$new(lower = 0, upper = 2, mode = 1, symmetric = FALSE)))
  expect_false(testSymmetric(Triangular$new(lower = 1, upper = 2, mode = 1.8, symmetric = FALSE)))
  expect_false(testSymmetric(Triangular$new(lower = 1, upper = 2)$setParameterValue(mode = 1.8)))
})
