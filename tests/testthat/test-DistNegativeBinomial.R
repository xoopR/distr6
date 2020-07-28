p <- 0.2
r <- 10

test_that("autotest", {
  autotest_sdistribution(
    sdist = NegativeBinomial,
    pars = list(prob = 0.2, form = "fbs"),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Naturals$new(),
    symmetry = "asymmetric",
    mean = (1 - p) * r / p,
    mode = floor((1 - p) * (r - 1) / (p)),
    median = qnbinom(0.5, r, p),
    variance = (1 - p) * r / (p)^2,
    skewness = (1 + (1 - p)) / sqrt((1 - p) * r),
    exkur = 6 / r + p^2 / ((1 - p) * r),
    mgf = (p / (1 - ((1 - p) * exp(1))))^r,
    cf = (p / (1 - ((1 - p) * exp(1i))))^r,
    pgf = 1,
    pdf = dnbinom(1:3, r, p),
    cdf = pnbinom(1:3, r, p),
    quantile = qnbinom(c(0.24, 0.42, 0.5), r, p)
  )
})

p <- 1 - p

test_that("autotest", {
  autotest_sdistribution(
    sdist = NegativeBinomial,
    pars = list(prob = 0.2, form = "sbf"),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Naturals$new(),
    symmetry = "asymmetric",
    mean = (1 - p) * r / p,
    mode = floor((1 - p) * (r - 1) / (p)),
    median = qnbinom(0.5, r, p),
    variance = (1 - p) * r / (p)^2,
    skewness = (1 + (1 - p)) / sqrt((1 - p) * r),
    exkur = 6 / r + p^2 / ((1 - p) * r),
    mgf = (p / (1 - ((1 - p) * exp(1))))^r,
    cf = (p / (1 - ((1 - p) * exp(1i))))^r,
    pgf = 1,
    pdf = dnbinom(1:3, r, p),
    cdf = pnbinom(1:3, r, p),
    quantile = qnbinom(c(0.24, 0.42, 0.5), r, p)
  )
})


p <- 1 - p

test_that("autotest", {
  autotest_sdistribution(
    sdist = NegativeBinomial,
    pars = list(prob = 0.2, form = "tbs"),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Interval$new(10, class = "integer", type = "[)"),
    symmetry = "asymmetric",
    mean = (1 - p) * r / p + r,
    mode = floor((1 - p) * (r - 1) / (p)) + r,
    median = qnbinom(0.5, r, p) + r,
    variance = (1 - p) * r / (p)^2,
    skewness = (1 + (1 - p)) / sqrt((1 - p) * r),
    exkur = 6 / r + p^2 / ((1 - p) * r),
    mgf = (p / (1 - ((1 - p) * exp(1))))^r,
    cf = (p / (1 - ((1 - p) * exp(1i))))^r,
    pgf = 1,
    pdf = rep(0, 3),
    cdf = rep(0, 3),
    quantile = qnbinom(c(0.24, 0.42, 0.5), r, p) + r
  )
})

p <- 1 - p

test_that("autotest", {
  autotest_sdistribution(
    sdist = NegativeBinomial,
    pars = list(prob = 0.2, form = "tbf"),
    traits = list(
      valueSupport = "discrete",
      variateForm = "univariate",
      type = Naturals$new()
    ),
    support = Interval$new(10, class = "integer", type = "[)"),
    symmetry = "asymmetric",
    mean = (1 - p) * r / p + r,
    mode = floor((1 - p) * (r - 1) / (p)) + r,
    median = qnbinom(0.5, r, p) + r,
    variance = (1 - p) * r / (p)^2,
    skewness = (1 + (1 - p)) / sqrt((1 - p) * r),
    exkur = 6 / r + p^2 / ((1 - p) * r),
    mgf = (p / (1 - ((1 - p) * exp(1))))^r,
    cf = (p / (1 - ((1 - p) * exp(1i))))^r,
    pgf = 1,
    pdf = rep(0, 3),
    cdf = rep(0, 3),
    quantile = qnbinom(c(0.24, 0.42, 0.5), r, p) + r
  )
})

test_that("manual", {
  dist <- NegativeBinomial$new(prob = 0.2, form = "tbs")
  expect_equal(dist$pdf(11:13), dnbinom(1:3, 10, 0.2))
  expect_equal(dist$cdf(11:13), pnbinom(1:3, 10, 0.2))
  dist <- NegativeBinomial$new(prob = 0.2, form = "tbf")
  expect_equal(dist$pdf(11:13), dnbinom(1:3, 10, 0.8))
  expect_equal(dist$cdf(11:13), pnbinom(1:3, 10, 0.8))
  expect_error(VectorDistribution$new(distribution = "Negative", params = data.frame(size = 1:2)),
               "should be used")
  expect_equal(dist$mgf(10), NaN)
  expect_equal(dist$pgf(10), NaN)
  expect_error(dist$setParameterValue(form = "tbf"), "settable after")
})
