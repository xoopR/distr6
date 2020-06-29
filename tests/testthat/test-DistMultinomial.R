library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Multinomial,
    pars = list(size = 2, probs = c(0.1, 0.9)),
    traits = list(
      valueSupport = "discrete",
      variateForm = "multivariate",
      type = Naturals$new()^2
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

test_that("manual", {
  expect_error(Multinomial$new(probs = 1), "Binomial")
})

# test_that("multivariate pdf", {
#   expect_equal(
#     Multinomial$new(probs = c(1, 4), size = 5)$pdf(c(1, 2, 0), c(4, 3, 5)),
#     c(
#       dmultinom(x = c(1, 4), prob = c(1, 4)),
#       dmultinom(x = c(2, 3), prob = c(1, 4)),
#       dmultinom(x = c(0, 5), prob = c(1, 4))
#     )
#   )
# })
