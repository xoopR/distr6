library(testthat)

context("Empirical distribution")

test_that("autotest", {
  autotest_sdistribution(sdist = Empirical,
                         pars = list(1:10),
                         traits = list(valueSupport = "discrete",
                                       variateForm = "univariate",
                                       type = Reals$new()),
                         support = Reals$new(),
                         symmetry = "asymmetric",
                         mean = mean(1:10),
                         mode = 1:10,
                         median = median.default(1:10),
                         variance = var(1:10) * 9 / 10,
                         skewness = 0,
                         exkur = -1.22,
                         entropy = 3.32,
                         mgf = 3484.377,
                         cf = -0.14 + 0.14i,
                         pgf = 1,
                         pdf = rep(1/10, 3),
                         cdf = (1:3)/10,
                         quantile = c(3,5,5)
  )
})

