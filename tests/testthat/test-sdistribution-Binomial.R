skip_if_distr_not_installed(Binomial)

test_that("autottest", {
  autotest_sdistribution(
    Binomial,
    pars = list(prob = 0.5, size = 10),
    traits = list(valueSupport = "discrete", variateForm = "univariate", type = Naturals$new()),
    support = Set$new(0:10, class = "integer"),
    symmetry = "symmetric",
    mean = 5,
    mode = 5,
    median = 5,
    variance = 2.5,
    skewness = 0,
    exkur = -0.2,
    entropy = 2.7081,
    mgf = 493.3136,
    cf = 0.0769 - 0.2598i,
    pgf = 1,
    pdf = dbinom(1:3, size = 10, prob = 0.5),
    cdf = pbinom(1:3, size = 10, prob = 0.5),
    quantile = qbinom(c(0.24, 0.42, 0.5), size = 10, prob = 0.5)
  )
})
