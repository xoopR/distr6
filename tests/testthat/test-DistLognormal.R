library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Lognormal,
    pars = list(meanlog = 0, varlog = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new()
    ),
    support = PosReals$new(),
    symmetry = "asymmetric",
    mean = exp(1 / 2),
    mode = 0.3678794,
    median = 1,
    variance = (exp(1) - 1) * exp(1),
    skewness = (exp(1) + 2) * sqrt(exp(1) - 1),
    exkur = exp(4) + 2 * exp(3) + 3 * exp(2) - 6,
    entropy = log(exp(0.5) * sqrt(2 * pi), 2),
    mgf = NaN,
    pgf = NaN,
    pdf = dlnorm(1:3),
    cdf = plnorm(1:3),
    quantile = qlnorm(c(0.24, 0.42, 0.5))
  )
})

test_that("autotest", {
  autotest_sdistribution(
    sdist = Lognormal,
    pars = list(mean = 1.648721, var = 4.670774),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new()
    ),
    support = PosReals$new(),
    symmetry = "asymmetric",
    mean = exp(1 / 2),
    mode = 0.3678794,
    median = 1,
    variance = (exp(1) - 1) * exp(1),
    skewness = (exp(1) + 2) * sqrt(exp(1) - 1),
    exkur = exp(4) + 2 * exp(3) + 3 * exp(2) - 6,
    entropy = log(exp(0.5) * sqrt(2 * pi), 2),
    mgf = NaN,
    pgf = NaN,
    pdf = dlnorm(1:3),
    cdf = plnorm(1:3),
    quantile = qlnorm(c(0.24, 0.42, 0.5))
  )
})

test_that("manual", {
  l <- Lognormal$new(meanlog = 1)$setParameterValue(sdlog = 2)
  expect_equal(l$getParameterValue("mean"), exp(3))
  expect_equal(l$getParameterValue("var"), (exp(4) - 1) * exp(6))
  expect_equal(l$getParameterValue("sd"), sqrt((exp(4) - 1) * exp(6)))
  expect_equal(l$getParameterValue("prec"), ((exp(4) - 1) * exp(6))^-1)
  l$setParameterValue(preclog = 0.25)
  expect_equal(l$getParameterValue("mean"), exp(3))
  expect_equal(l$getParameterValue("var"), (exp(4) - 1) * exp(6))
  expect_equal(l$getParameterValue("sd"), sqrt((exp(4) - 1) * exp(6)))
  expect_equal(l$getParameterValue("prec"), ((exp(4) - 1) * exp(6))^-1)
})
