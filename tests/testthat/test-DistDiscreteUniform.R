library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = DiscreteUniform,
    pars = list(lower = 0, upper = 10),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Integers$new()
    ),
    support = Interval$new(0, 10, class = "integer"),
    symmetry = "symmetric",
    mean = 5,
    mode = 0:10,
    median = 5,
    variance = 10,
    skewness = 0,
    exkur = -1.22,
    entropy = 3.4594,
    mgf = (1 - exp(11)) / (11 * (1 - exp(1))),
    cf = -0.0379498 + 0.1282899i,
    pgf = 1,
    pdf = extraDistr::ddunif(1:3, 0, 10),
    cdf = extraDistr::pdunif(1:3, 0, 10),
    quantile = extraDistr::qdunif(c(0.24, 0.42, 0.5), 0, 10)
  )
})

test_that("manual", {
  d <- VectorDistribution$new(distribution = "DiscreteUnif",
                              params = data.frame(lower = 1:2, upper = 3:4))
  expect_error(d$mode(), "cannot be")
  expect_equal(d$mode(100), c(DUnif1 = 3, DUnif2 = 4))
})
