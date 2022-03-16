skip_if_distr_not_installed(Matdist)

wd1 <- WeightedDiscrete$new(x = 1:2, pdf = c(0.1, 0.9))
wd2 <- WeightedDiscrete$new(x = 1:2, pdf = c(0.7, 0.3))

test_that("autottest", {
  autotest_sdistribution(Matdist,
    pars = list(pdf = matrix(c(0.1, 0.7, 0.9, 0.3), 2, 2, FALSE, list(NULL, 1:2))),
    traits = list(
      valueSupport = "discrete", variateForm = "univariate",
      type = Reals$new()^"n"
    ),
    support = Set$new(1:2, class = "numeric"),
    symmetry = "asymmetric",
    mean = c(wd1$mean(), wd2$mean()),
    mode = c(wd1$mode(), wd2$mode()),
    median = c(wd1$median(), wd2$median()),
    variance = c(wd1$variance(), wd2$variance()),
    skewness = c(wd1$skewness(), wd2$skewness()),
    exkur = c(wd1$kurtosis(), wd2$kurtosis()),
    entropy = c(wd1$entropy(), wd2$entropy()),
    mgf = c(wd1$mgf(1), wd2$mgf(1)),
    cf = c(wd1$cf(1), wd2$cf(1)),
    pgf = c(wd1$pgf(1), wd2$pgf(1)),
    pdf = matrix(c(wd1$pdf(1:3), wd2$pdf(1:3)), 2, 3, TRUE, list(NULL, 1:3)),
    cdf = matrix(c(wd1$cdf(1:3), wd2$cdf(1:3)), 2, 3, TRUE, list(NULL, 1:3)),
    quantile = matrix(c(wd1$quantile(c(0.24, 0.42, 0.5)), wd2$quantile(c(0.24, 0.42, 0.5))),
                      2, 3, TRUE, dimnames = NULL),
    vectorise = FALSE
  )
})

wd1 <- WeightedDiscrete$new(x = 1:2, pdf = c(0.1, 0.8))
wd2 <- WeightedDiscrete$new(x = 1:2, pdf = c(0.7, 0.3))


test_that("autottest improper", {
  autotest_sdistribution(Matdist,
    pars = list(pdf = matrix(c(0.1, 0.7, 0.8, 0.3), 2, 2, FALSE, list(NULL, 1:2))),
    traits = list(
      valueSupport = "discrete", variateForm = "univariate",
      type = Reals$new()^"n"
    ),
    support = Set$new(1:2, class = "numeric"),
    symmetry = "asymmetric",
    mean = c(wd1$mean(), wd2$mean()),
    mode = c(wd1$mode(), wd2$mode()),
    median = c(wd1$median(), wd2$median()),
    variance = c(wd1$variance(), wd2$variance()),
    skewness = c(wd1$skewness(), wd2$skewness()),
    exkur = c(wd1$kurtosis(), wd2$kurtosis()),
    entropy = c(wd1$entropy(), wd2$entropy()),
    mgf = c(wd1$mgf(1), wd2$mgf(1)),
    cf = c(wd1$cf(1), wd2$cf(1)),
    pgf = c(wd1$pgf(1), wd2$pgf(1)),
    pdf = matrix(c(wd1$pdf(1:3), wd2$pdf(1:3)), 2, 3, TRUE, list(NULL, 1:3)),
    cdf = matrix(c(wd1$cdf(1:3), wd2$cdf(1:3)), 2, 3, TRUE, list(NULL, 1:3)),
    quantile = matrix(c(wd1$quantile(c(0.24, 0.42, 0.5)), wd2$quantile(c(0.24, 0.42, 0.5))),
                      2, 3, TRUE, dimnames = NULL),
    vectorise = FALSE
  )
})
