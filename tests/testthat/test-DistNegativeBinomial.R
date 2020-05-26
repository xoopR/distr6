library(testthat)

context("NegativeBinomial distribution")

test_that("autotest", {
  autotest_sdistribution(sdist = NegativeBinomial,
                         pars = list(form = "fbs", prob = 0.2),
                         traits = list(valueSupport = "discrete",
                                       variateForm = "univariate",
                                       type = Naturals$new()),
                         support = Naturals$new(),
                         symmetry = "asymmetric",
                         mean =  8 / 0.2,
                         mode = 36,
                         median = qnbinom(0.5, 10, 0.2),
                         variance = 8 / 0.04,
                         skewness = 1.8 / sqrt(8),
                         exkur = 0.605,
                         mgf = (0.2 / (1 - (0.8 * exp(1))))^10,
                         cf = (0.2 / (1 - (0.8 * exp(1i))))^10,
                         pgf = 1,
                         pdf = dnbinom(1:3, 10, 0.2),
                         cdf = pnbinom(1, 10, 0.2),
                         quantile = qnbinom(c(0.24, 0.42, 0.5), 10, 0.2)
  )
})

