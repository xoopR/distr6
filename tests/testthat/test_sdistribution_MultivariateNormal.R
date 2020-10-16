library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = MultivariateNormal,
    pars = list(mean = c(1, 7), cov = c(1, 0, 0, 1)),
    traits = list(
      valueSupport = "continuous",
      variateForm = "multivariate",
      type = Reals$new()^2
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
  expect_error(MultivariateNormal$new(mean = 1), "Normal")
  expect_equal(MultivariateNormal$new()$getParameterValue("prec"), matrix(c(1, 0, 0, 1), 2, 2))
  expect_equal(MultivariateNormal$new(cov = c(1, 2, 2, 2))$pdf(2, 2), NaN)
})

#
#
# test_that("multivariate pdf", {
#   expect_equal(
#     signif(MultivariateNormal$new(mean = c(1, 7, 3), cov = c(1, 0, 0, 0, 1, 0, 0, 0, 1))$
#       pdf(1:2, 2:3, 3:4), 3),
#     c(2.366e-07, 7.835e-06)
#   )
# })
