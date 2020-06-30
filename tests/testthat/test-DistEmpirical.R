library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = Empirical,
    pars = list(1:10),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Reals$new()
    ),
    support = Tuple$new(1:10, universe = Reals$new(), class = "numeric"),
    symmetry = "asymmetric",
    mean = mean(1:10),
    mode = 1:10,
    median = 5,
    variance = var(1:10) * 9 / 10,
    skewness = 0,
    exkur = -1.2242,
    entropy = 3.321928,
    mgf = sum(exp(1:10) * (1 / 10)),
    cf = sum(exp(1i * 1:10) * (1 / 10)),
    pgf = 1,
    pdf = rep(1 / 10, 3),
    cdf = (1:3) / 10,
    quantile = c(3, 5, 5),
    vectorise = FALSE
  )
})

test_that("manual", {
  dist <- Empirical$new(1:10)
  expect_equal(dist$mode(which = 2), 2)
  expect_equal(dist$mgf(1:3), c(dist$mgf(1), dist$mgf(2), dist$mgf(3)))
  expect_equal(dist$pgf(1:3), c(dist$pgf(1), dist$pgf(2), dist$pgf(3)))
  expect_equal(dist$cf(1:3), c(dist$cf(1), dist$cf(2), dist$cf(3)))
  expect_null(expect_warning(dist$setParameterValue(sd = 2), "Data cannot"))
})
