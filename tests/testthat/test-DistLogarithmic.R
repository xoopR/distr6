library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Logarithmic,
    pars = list(theta = 0.5),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = PosNaturals$new()
    ),
    support = PosNaturals$new(),
    symmetry = "asymmetric",
    mean = -1 / log(0.5),
    mode = 1,
    median = extraDistr::qlgser(0.5, 0.5),
    variance = (-0.25 - 0.5 * log(0.5)) / (0.25 * log(0.5)^2),
    skewness = 3.0148,
    exkur = 13.3884,
    mgf = NaN,
    cf = 0.2473617 + 0.7544398i,
    pgf = 1,
    pdf = extraDistr::dlgser(1:3, 0.5),
    cdf = extraDistr::plgser(1:3, 0.5),
    quantile = extraDistr::qlgser(c(0.24, 0.42, 0.5), 0.5)
  )
})

test_that("manual", {
  expect_equal(Logarithmic$new()$mgf(0.5), log(1 - 0.5 * exp(0.5)) / log(1 - 0.5))
  expect_equal(Logarithmic$new()$pgf(2), NaN)
})
