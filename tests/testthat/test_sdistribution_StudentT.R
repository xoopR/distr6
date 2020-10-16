library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = StudentT,
    pars = list(df = 1),
    traits = list(valueSupport = "continuous", variateForm = "univariate", type = Reals$new()),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = NaN,
    mode = 0,
    median = NaN,
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    entropy = digamma(1) - digamma(1 / 2) + log(beta(1 / 2, 1 / 2), 2),
    mgf = NaN,
    cf = besselK(1, 1 / 2) / (gamma(1 / 2) * 2^(-1 / 2)), # nolint
    pgf = NaN,
    pdf = dt(1:3, 1),
    cdf = pt(1:3, 1),
    quantile = qt(c(0.24, 0.42, 0.5), 1)
  )
})
