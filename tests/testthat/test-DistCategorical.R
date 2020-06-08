library(testthat)

context("Categorical distribution")

test_that("autottest", {
  autotest_sdistribution(Categorical,
    pars = list(1, 2, 3, probs = c(0.02, 0.18, 0.80)),
    traits = list(valueSupport = "discrete", variateForm = "univariate", type = UniversalSet$new()),
    support = Set$new(1, 2, 3),
    symmetry = "asymmetric",
    mean = NaN,
    mode = 3,
    median = 3,
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    entropy = NaN,
    mgf = NaN,
    cf = NaN,
    pgf = NaN,
    pdf = c(0.02, 0.18, 0.80),
    cdf = c(0.02, 0.20, 1),
    quantile = c(3, 3, 3)
  )
})
