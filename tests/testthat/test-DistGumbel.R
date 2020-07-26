test_that("autotest", {
  expect_warning({
    autotest_sdistribution(
      sdist = Gumbel,
      pars = list(location = 0, scale = 1),
      traits = list(
        valueSupport = "continuous",
        variateForm = "univariate",
        type = Reals$new()
      ),
      support = Reals$new(),
      symmetry = "asymmetric",
      mean = -digamma(1),
      mode = 0,
      median = extraDistr::qgumbel(0.5),
      variance = pi^2 / 6,
      skewness = 1.139547,
      exkur = 12 / 5,
      entropy = log(1, 2) - digamma(1) + 1,
      mgf = NaN,
      cf = 0.4980157 + 0.1549498i,
      pgf = NaN,
      pdf = extraDistr::dgumbel(1:3),
      cdf = extraDistr::pgumbel(1:3),
      quantile = extraDistr::qgumbel(c(0.24, 0.42, 0.5))
    )
  })
})
