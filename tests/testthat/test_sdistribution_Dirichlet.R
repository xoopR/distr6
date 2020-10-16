library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Dirichlet,
    pars = list(params = c(2, 3)),
    traits = list(
      valueSupport = "continuous",
      variateForm = "multivariate",
      type = Interval$new(0, 1, type = "()")^2
    ),
    support = Interval$new(0, 1, type = "()")^2,
    symmetry = "asymmetric",
    mean = c(2 / 5, 3 / 5),
    mode = c(1 / 3, 2 / 3),
    median = NaN,
    variance = matrix(c(0.04, -0.04, -0.04, 0.04), nrow = 2),
    entropy = round(log(1 / 12, base = 2) + 3 * digamma(5) - 2.268353, 5),
    pgf = NaN
  )
})

# test_that("multivariate pdf", {
#   expect_equal(
#     Dirichlet$new(c(2, 3))$pdf(c(0.1, 0.2), c(0.3, 0.4)),
#     c(
#       extraDistr::ddirichlet(c(0.1, 0.3), alpha = c(2, 3)),
#       extraDistr::ddirichlet(c(0.2, 0.4), alpha = c(2, 3))
#     )
#   )
# })
