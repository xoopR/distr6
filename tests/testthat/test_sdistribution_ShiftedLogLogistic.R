library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = ShiftedLoglogistic,
    pars = list(shape = 2, scale = 3, location = 1),
    traits = list(
      valueSupport = "continuous",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Interval$new(-0.5, Inf, type = "[)"),
    symmetry = "asymmetric",
    mean = 1 + (3 / 2) * (pi * 2 / sin(pi * 2) - 1),
    mode = -0.3333333,
    median = 1,
    variance = (9 / 4) * ((4 * pi / sin(4 * pi)) - (2 * pi / sin(2 * pi))^2),
    pgf = NaN,
    pdf = c(0.08333333, 0.04919334, 0.03415854),
    cdf = c(0.5000, 0.5635, 0.6044),
    quantile = c(-0.3504155, 0.2865636, 1.0000000),
  )
})

test_that("manual", {
  dist <- ShiftedLoglogistic$new()
  expect_equal(dist$pdf(-2, log = F), 0)
  expect_equal(dist$pdf(-2, log = T), -Inf)
  expect_equal(dist$cdf(-2, log.p = F, lower.tail = T), 0)
  expect_equal(dist$cdf(-2, log.p = F, lower.tail = F), 1)
  expect_equal(dist$cdf(-2, log.p = T, lower.tail = T), -Inf)
  expect_equal(dist$cdf(-2, log.p = T, lower.tail = F), 0)
  expect_equal(ShiftedLoglogistic$new(shape = 0)$properties$support, Reals$new())
  expect_equal(ShiftedLoglogistic$new(shape = 1)$properties$support,
               Interval$new(-1, Inf, type = "[)"))
  expect_equal(ShiftedLoglogistic$new(shape = -1)$properties$support,
               Interval$new(-Inf, 1, type = "(]"))
  expect_equal(dist$setParameterValue(shape = 0)$properties$support,
               Reals$new())
  expect_equal(dist$setParameterValue(shape = 1)$properties$support,
               Interval$new(-1, Inf, type = "[)"))
  expect_equal(dist$setParameterValue(shape = -1)$properties$support,
               Interval$new(-Inf, 1, type = "(]"))
})

test_that("cpp", {
  expect_equal(as.numeric(C_ShiftedLoglogisticQuantile(-2, 0, 1, 1, TRUE, FALSE)), NaN)
})
