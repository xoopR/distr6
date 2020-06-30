library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = FDistribution,
    pars = list(df1 = 2, df2 = 10),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 1.25,
    mode = NaN,
    median = 0.7435,
    variance = 2.6042,
    skewness = 4.6476,
    exkur = 70.8,
    entropy = -4.6439,
    mgf = NaN,
    pgf = NaN,
    pdf = df(1:3, 2, 10),
    cdf = pf(1:3, 2, 10),
    quantile = qf(c(0.24, 0.42, 0.5), 2, 10)
  )
})
