skip_if_distr_not_installed(WeightedDiscrete)

test_that("autottest", {
  autotest_sdistribution(WeightedDiscrete,
    pars = list(x = 1:10, pdf = rep(0.1, 10)),
    traits = list(
      valueSupport = "discrete", variateForm = "univariate",
      type = Reals$new()
    ),
    support = Set$new(1:10, class = "numeric"),
    symmetry = "asymmetric",
    mean = mean(1:10),
    mode = 1:10,
    median = 5,
    variance = 8.25,
    skewness = 0,
    exkur = sum(((1:10 - 5.5) / sqrt(8.25))^4 * 0.1) - 3,
    entropy = 3.3219,
    mgf = sum(exp(1:10) * (0.1)),
    cf = -0.1417448 + 0.1411188i,
    pgf = 1,
    pdf = rep(1 / 10, 3),
    cdf = c(1 / 10, 2 / 10, 3 / 10),
    quantile = c(3, 5, 5)
  )
})

test_that("setting", {
  x <- WeightedDiscrete$new(x = 0:5, pdf = dgeom(0:5, 0.5))
  expect_equal(x$getParameterValue("pdf"), dgeom(0:5, 0.5))
  expect_equal(x$getParameterValue("cdf"), pgeom(0:5, 0.5))
  expect_silent(x$setParameterValue(pdf = dbinom(0:5, 10, 0.5)))
  expect_equal(x$getParameterValue("pdf"), dbinom(0:5, 10, 0.5))
  expect_equal(x$getParameterValue("cdf"), pbinom(0:5, 10, 0.5))
})

test_that("manual", {
  v <- VectorDistribution$new(distribution = "Weigh", params = data.frame(x = 1:2, pdf = 1))
  expect_error(v$mode(), "cannot be")
  expect_equal(v$mode(100), c(WeightDisc1 = 1, WeightDisc2 = 2))
  expect_equal(v$cdf(3), data.table(WeightDisc1 = 1, WeightDisc2 = 1))
  expect_equal(v$cdf(3, lower.tail = FALSE, log.p = TRUE),
               data.table(WeightDisc1 = -Inf, WeightDisc2 = -Inf))
})

test_that("autottest improper", {
  autotest_sdistribution(WeightedDiscrete,
    pars = list(x = 1:3, cdf = c(0.1, 0.2, 0.4)),
    traits = list(
      valueSupport = "discrete", variateForm = "univariate",
      type = Reals$new()
    ),
    support = Set$new(1:3, class = "numeric"),
    symmetry = "asymmetric",
    mean = Inf,
    mode = 3,
    median = NaN,
    variance = Inf,
    skewness = Inf,
    exkur = Inf,
    entropy = Inf,
    mgf = Inf,
    cf = Inf,
    pgf = Inf,
    pdf = c(0.1, 0.1, 0.2),
    cdf = c(0.1, 0.2, 0.4),
    quantile = c(3, NA, NA)
  )
})
