library(testthat)

context("Pareto distribution")

test_that("autotest", {
  autotest_sdistribution(sdist = Pareto,
                         pars = list(),
                         traits = list(valueSupport = "continuous",
                                       variateForm = "univariate",
                                       type = PosReals$new(zero = T)),
                         support = Interval$new(lower = 1, type = "[)"),
                         symmetry = "asymmetric",
                         mean = Inf,
                         mode = 1,
                         median = extraDistr::qpareto(0.5),
                         variance = Inf,
                         skewness = NaN,
                         exkur = NaN,
                         entropy = log(exp(2), base = 2),
                         mgf = NaN,
                         pgf = NaN,
                         pdf = extraDistr::dpareto(1:3),
                         cdf = extraDistr::ppareto(1:3),
                         quantile = extraDistr::qpareto(c(0.24, 0.42, 0.5))
  )
})

