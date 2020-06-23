library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = FDistributionNoncentral,
    pars = list(df1 = 2, df2 = 10, location = 2),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = 2.5,
    median = 1.6455,
    variance = 8.3333,
    pdf = df(1:3, 2, 10, 2),
    cdf = pf(1:3, 2, 10, 2),
    quantile = qf(c(0.24, 0.42, 0.5), 2, 10, 2)
  )
})
