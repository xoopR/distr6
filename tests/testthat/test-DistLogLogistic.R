library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Loglogistic,
    pars = list(shape = 2, scale = 3),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = PosReals$new(zero = TRUE)
    ),
    support = PosReals$new(zero = TRUE),
    symmetry = "asymmetric",
    mean = (3 * pi / 2) / sin(pi / 2),
    mode = 1.73205,
    median = actuar::qllogis(0.5, shape = 2, scale = 3),
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    pgf = NaN,
    pdf = actuar::dllogis(1:3, shape = 2, scale = 3),
    cdf = actuar::pllogis(1:3, shape = 2, scale = 3),
    quantile = actuar::qllogis(c(0.24, 0.42, 0.5), shape = 2, scale = 3),
  )
})
