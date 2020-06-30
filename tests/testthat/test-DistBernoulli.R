library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Bernoulli,
    pars = list(prob = 0.2),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Set$new(0, 1, class = "integer"),
    symmetry = "asymmetric",
    mean = 0.2,
    mode = 0,
    median = 0,
    variance = 0.2 * 0.8,
    skewness = 1.5,
    exkur = 0.25,
    entropy = 0.7219,
    mgf = 0.8 + 0.2 * exp(1),
    cf = 0.8 + 0.2 * exp(1i),
    pgf = 1,
    pdf = dbinom(1:3, 1, 0.2),
    cdf = pbinom(1:3, 1, 0.2),
    quantile = qbinom(c(0.24, 0.42, 0.5), 1, 0.2)
  )
})

test_that("manual", {
  expect_equal(Bernoulli$new(1)$mode(), 1)
  expect_equal(Bernoulli$new(0.5)$mode(), c(0, 1))
  expect_equal(Bernoulli$new(0.5)$mode(1), 0)
})

test_that("vector", {
  d <- VectorDistribution$new(distribution = "Bernoulli",
                              params = data.frame(prob = (1:2) / 3))
  expect_error(d$mode(), "cannot be")
})
