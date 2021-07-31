library(testthat)

test_that("autotest", {
  expect_warning({
    autotest_sdistribution(
      sdist = Wald,
      pars = list(mean = 2.5, shape = 3),
      traits = list(
        valueSupport = "continuous",
        variateForm = "univariate",
        type = PosReals$new()
      ),
      support = PosReals$new(),
      symmetry = "asymmetric",
      mean = 2.5,
      mode = 2.5 * (sqrt(2.5625) - 1.25),
      median = NULL,
      variance = 2.5^3 / 3,
      skewness = 2.738613,
      exkur = 12.5,
      mgf = NaN,
      cf = 0.0155173 + 0.4717857i,
      pgf = NaN,
      pdf = extraDistr::dwald(1:3, 2.5, 3),
      cdf = extraDistr::pwald(1:3, 2.5, 3)
    )
  })
})
