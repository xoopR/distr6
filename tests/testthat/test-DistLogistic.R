library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Logistic,
    pars = list(mean = 0, scale = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = 0,
    mode = 0,
    median = 0,
    variance = pi^2 / 3,
    skewness = 0,
    exkur = 1.2,
    entropy = 2,
    mgf = NaN,
    cf = as.complex(pi / sinh(pi)),
    pgf = NaN,
    pdf = dlogis(1:3),
    cdf = plogis(1:3),
    quantile = qlogis(c(0.24, 0.42, 0.5))
  )
})

test_that("manual", {
  expect_equal(Logistic$new()$mgf(0.5), beta(0.5, 1.5))
})
