library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Arcsine,
    pars = list(lower = 0, upper = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Interval$new(0, 1),
    symmetry = "symmetric",
    mean = 0.5,
    mode = 0:1,
    median = 0.5,
    variance = 1 / 8,
    skewness = 0,
    exkur = -1.5,
    entropy = log(pi / 4, 2),
    pgf = NaN,
    pdf = dbeta(1:3, 0.5, 0.5),
    cdf = pbeta(1:3, 0.5, 0.5),
    quantile = qbeta(c(0.24, 0.42, 0.5), 0.5, 0.5)
  )
})

test_that("manual", {
  dist <- Arcsine$new(lower = 0, upper = 1)
  expect_equal(dist$pdf(0.5), 1 / (pi * sqrt(0.25)))
  expect_equal(dist$pdf(0.5, log = TRUE), -log(pi * sqrt(0.25)))
  expect_equal(dist$cdf(0.5), 2 / pi * asin(sqrt(0.5)))
  expect_equal(dist$quantile(c(0, 1)), c(0, 1))
})

test_that("vector", {
  d <- VectorDistribution$new(distribution = "Arcsine",
                              params = data.frame(lower = 1:2, upper = 3:4))
  expect_equal(d$mode(), data.table(Arc1 = list(1, 3), Arc2 = list(2, 4)))
})

test_that("cpp", {
  expect_equal(as.numeric(C_ArcsineCdf(-2, 0, 1, TRUE, FALSE)), 0)
  expect_equal(as.numeric(C_ArcsineQuantile(-2, 0, 1, TRUE, FALSE)), NaN)
  expect_equal(as.numeric(C_ArcsineQuantile(0, 0, 1, TRUE, FALSE)), 0)
  expect_equal(as.numeric(C_ArcsineQuantile(1, 0, 1, TRUE, FALSE)), 1)
})
