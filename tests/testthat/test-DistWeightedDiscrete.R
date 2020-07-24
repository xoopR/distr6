library(testthat)

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
  expect_silent(x$setParameterValue(cdf = pbinom(0:5, 10, 0.5)))
  expect_equal(x$getParameterValue("pdf"), dbinom(0:5, 10, 0.5))
  expect_equal(x$getParameterValue("cdf"), pbinom(0:5, 10, 0.5))
})

test_that("manual", {
  expect_warning(WeightedDiscrete$new(data = data.frame(x = 1, pdf = 1)), "deprecated")
  w <- WeightedDiscrete$new(x = 1:5, pdf = dnorm(1:5))
  expect_equal(w$mgf(1:3), c(w$mgf(1), w$mgf(2), w$mgf(3)))
  expect_equal(w$cf(1:3), c(w$cf(1), w$cf(2), w$cf(3)))
  expect_equal(w$pgf(1:3), c(w$pgf(1), w$pgf(2), w$pgf(3)))
  expect_equal(w$cdf(6), 1)
  expect_equal(w$cdf(6, lower.tail = FALSE, log.p = TRUE), -Inf)
  expect_equal(w$cdf(0), 0)
  v <- VectorDistribution$new(distribution = "Weigh", params = data.frame(x = 1:2, pdf = 1))
  expect_error(v$mode(), "cannot be")
  expect_equal(v$mode(100), c(WeightDisc1 = 1, WeightDisc2 = 2))
  expect_equal(v$cdf(3), data.table(WeightDisc1 = 1, WeightDisc2 = 1))
  expect_equal(v$cdf(3, lower.tail = FALSE, log.p = TRUE),
               data.table(WeightDisc1 = -Inf, WeightDisc2 = -Inf))
  expect_output(w$print(), "WeightDisc()")
})
