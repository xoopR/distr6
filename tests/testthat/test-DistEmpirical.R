library(testthat)

test_that("autotest", {
  autotest_sdistribution(sdist = Empirical,
                         pars = list(1:10),
                         traits = list(valueSupport = "discrete",
                                       variateForm = "univariate",
                                       type = Reals$new()),
                         support = Tuple$new(1:10, universe = Reals$new(), class = "numeric"),
                         symmetry = "asymmetric",
                         mean = mean(1:10),
                         mode = 1:10,
                         median = 5,
                         variance = var(1:10) * 9 / 10,
                         skewness = 0,
                         exkur = -1.2242,
                         entropy = 3.321928,
                         mgf = sum(exp(1:10) * (1 / 10)),
                         cf = sum(exp(1i*1:10) * (1 / 10)),
                         pgf = 1,
                         pdf = rep(1/10, 3),
                         cdf = (1:3)/10,
                         quantile = c(3,5,5)
  )
})

