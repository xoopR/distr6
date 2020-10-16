library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = StudentTNoncentral,
    pars = list(df = 2, location = 1),
    traits = list(valueSupport = "continuous", variateForm = "univariate", type = Reals$new()),
    support = Reals$new(),
    symmetry = "symmetric",
    mean = 1.7725,
    median = 1.7725,
    variance = NaN,
    pdf = dt(1:3, 2, 1),
    cdf = pt(1:3, 2, 1),
    quantile = qt(c(0.24, 0.42, 0.5), 2, 1)
  )
})
