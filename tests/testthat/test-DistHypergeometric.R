library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Hypergeometric,
    pars = list(size = 50, successes = 5, draws = 10),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Set$new(0:5, class = "integer"),
    symmetry = "asymmetric",
    mean = 1,
    mode = 1,
    median = qhyper(0.5, 5, 50, 10),
    variance = 0.7346939,
    skewness = 0.5833333,
    exkur = -0.0750591,
    pdf = dhyper(1:3, 5, 45, 10),
    cdf = phyper(1:3, 5, 45, 10),
    quantile = qhyper(c(0.24, 0.42, 0.5), 5, 50, 10)
  )
})
