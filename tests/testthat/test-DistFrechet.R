library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Frechet,
    pars = list(shape = 1, scale = 1, minimum = 0),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Interval$new(0, Inf, type = "()"),
    symmetry = "asymmetric",
    mean = Inf,
    mode = 1 / 2,
    median = 1.4427,
    variance = Inf,
    skewness = Inf,
    exkur = Inf,
    entropy = 1 - 2 * digamma(1),
    pgf = NaN,
    pdf = extraDistr::dfrechet(1:3),
    cdf = extraDistr::pfrechet(1:3),
    quantile = extraDistr::qfrechet(c(0.24, 0.42, 0.5))
  )
})
