test_that("autottest", {
  autotest_sdistribution(Categorical,
    pars = list(elements = list(1, 2, 3), probs = c(0.02, 0.18, 0.80)),
    traits = list(valueSupport = "discrete", variateForm = "univariate", type = Universal$new()),
    support = Set$new(1, 2, 3),
    symmetry = "asymmetric",
    mean = NaN,
    mode = 3,
    median = 3,
    variance = NaN,
    skewness = NaN,
    exkur = NaN,
    entropy = NaN,
    mgf = NaN,
    cf = NaN,
    pgf = NaN,
    pdf = c(0.02, 0.18, 0.80),
    cdf = c(0.02, 0.20, 1),
    quantile = c(3, 3, 3)
  )
})

test_that("vector", {
  d <- VectorDistribution$new(distribution = "Categorical",
                              params = list(list(elements = 1, probs = 1),
                                            list(elements = 2, probs = 1)))
  expect_error(d$mode(), "cannot be")
  expect_equal(d$mode(100), c(Cat1 = 1, Cat2 = 2))
})

test_that("manual", {
  expect_equal(Categorical$new(elements = 1:2,
                               probs = 1:2)$ # nolint
                 setParameterValue(elements = list(1, "a"), probs = c(0.5, 0.5))$ # nolint
                 getParameterValue("elements"), list(1, "a"))
})
