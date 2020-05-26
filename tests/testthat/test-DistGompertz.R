library(testthat)

context("Gompertz distribution")

test_that("autotest", {
  autotest_sdistribution(sdist = Gompertz,
                         pars = list(),
                         traits = list(valueSupport = "continuous",
                                       variateForm = "univariate",
                                       type = PosReals$new(zero = TRUE)),
                         support = PosReals$new(zero = TRUE),
                         symmetry = "asymmetric",
                         median = extraDistr::qgompertz(0.5),
                         pgf = NaN,
                         pdf = extraDistr::dgompertz(1:3),
                         cdf = extraDistr::pgompertz(1:3),
                         quantile = extraDistr::qgompertz(c(0.24, 0.42, 0.5))
  )
})
