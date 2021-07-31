library(testthat)

test_that("autotest_failures", {
  autotest_sdistribution(
    sdist = Geometric,
    pars = list(prob = 0.5),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Naturals$new(),
    symmetry = "asymmetric",
    mean = 1,
    mode = 0,
    median = 0,
    variance = 2,
    skewness = 1.5 / sqrt(0.5),
    exkur = 6.5,
    entropy = (-0.5 * log(0.5, base = 2) - 0.5 * log(0.5, base = 2)) * 2,
    mgf = 0.5 / (1 - 0.5 * exp(1)),
    cf = 0.5 / (1 - 0.5 * exp(1i)),
    pgf = 1,
    pdf = dgeom(1:3, 0.5),
    cdf = pgeom(1:3, 0.5),
    quantile = qgeom(c(0.24, 0.42, 0.5), 0.5)
  )
})

test_that("autotest_trials", {
  autotest_sdistribution(
    sdist = Geometric,
    pars = list(prob = 0.5, trials = TRUE),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = PosNaturals$new(),
    symmetry = "asymmetric",
    mean = 2,
    mode = 1,
    median = 1,
    variance = 2,
    skewness = 1.5 / sqrt(0.5),
    exkur = 6.5,
    entropy = (-0.5 * log(0.5, base = 2) - 0.5 * log(0.5, base = 2)) * 2,
    mgf = NaN,
    cf = 0.5 * exp(1i) / (1 - 0.5 * exp(1i)),
    pgf = 1,
    pdf = dgeom(2:4, 0.5),
    cdf = pgeom(2:4, 0.5),
    quantile = qgeom(c(0.24, 0.42, 0.5), 0.5) + 1
  )
})

test_that("manual", {
  expect_equal(Geometric$new(trials = TRUE)$mgf(0.5), 0.5 * exp(0.5) / (1 - 0.5 * exp(0.5)))
})
