test_that("autotest", {
  autotest_sdistribution(
    sdist = MultivariateNormal,
    pars = list(mean = c(1, 7), cov = c(1, 0, 0, 1)),
    traits = list(
      valueSupport = "continuous",
      variateForm = "multivariate",
      type = Reals$new()^"n"
    ),
    support = Reals$new()^2,
    symmetry = "asymmetric",
    mean = c(1, 7),
    mode = c(1, 7),
    median = NaN,
    variance = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2),
    entropy = 4.094191,
    mgf = 39824784,
    cf = -9.254893 + 7.922128i,
    pgf = NaN
  )
})

test_that("manual", {
  expect_silent(MultivariateNormal$new(mean = c(1, 1), prec = c(1, 0, 0, 1)))
  expect_equal(MultivariateNormal$new()$getParameterValue("prec"), matrix(c(1, 0, 0, 1), 2, 2))
  expect_equal(MultivariateNormal$new(cov = c(1, 2, 2, 2))$pdf(2, 2), NaN)
})
