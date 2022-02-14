skip_if_distr_not_installed(Multinomial)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Multinomial,
    pars = list(size = 2, probs = c(0.1, 0.9)),
    traits = list(
      valueSupport = "discrete",
      variateForm = "multivariate",
      type = Naturals$new()^"n"
    ),
    support = Set$new(0:2, class = "integer")^2,
    symmetry = "asymmetric",
    mean = 2 * c(0.1, 0.9),
    median = NaN,
    variance = matrix(c(0.18, -0.18, -0.18, 0.18), nrow = 2, ncol = 2),
    skewness = NaN,
    exkur = NaN,
    entropy = 0.7579912,
    mgf = 47.91379,
    cf = -0.7118115 - 0.5785154i,
    pgf = 3.61
  )
})
