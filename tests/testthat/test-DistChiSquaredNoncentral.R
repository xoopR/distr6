library(testthat)

context("Noncentral ChiSquared distribution")

test_that("autotest", {
  autotest_sdistribution(sdist = ChiSquaredNoncentral,
                         pars = list(df = 8, location = 2),
                         traits = list(valueSupport = "continuous",
                                       variateForm = "univariate",
                                       type = PosReals$new(zero = TRUE)),
                         support = PosReals$new(zero = TRUE),
                         symmetry = "asymmetric",
                         mean = 10,
                         median = 9.2271,
                         variance = 24,
                         skewness = 0.9526,
                         exkur = 1.3333,
                         mgf = NaN,
                         cf = 0.0021 - 0.0179i,
                         pdf = dchisq(1:3, 8, 2),
                         cdf = pchisq(1:3, 8, 2),
                         quantile = qchisq(c(0.24, 0.42, 0.5), 8, 2)
  )
})
