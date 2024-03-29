library(testthat)



test_that("lists", {
  expect_silent(listDistributions())
  expect_silent(listDistributions(simplify = T))
  expect_silent(listDistributions(filter = list(VariateForm = "univariate")))
  expect_silent(listDistributions(filter = list(VariateForm = "matrixvariate")))
  expect_silent(listDistributions(filter = list(VariateForm = "multivariate")))
  expect_silent(listDistributions(filter = list(VariateForm = "discrete")))
  expect_silent(listDistributions(filter = list(VariateForm = "mixture")))
  expect_silent(listDistributions(filter = list(package = "distr6")))
  expect_silent(listDistributions(filter = list(VariateForm = "univariate",
                                                valuesupport = "continuous")))
  expect_silent(listDistributions(filter = list(efsf = "fsdf")))
  expect_silent(listDistributions(filter = list(Tags = "scale")))
  expect_silent(listDecorators(F))
  expect_silent(listDecorators(T))
  expect_silent(listWrappers(T))
  expect_silent(listWrappers(F))
  expect_silent(listKernels(T))
  expect_silent(listKernels(F))
})

test_that("unique distributions", {
  expect_silent(makeUniqueDistributions(list(Binomial$new(), Binomial$new()))$Binom1)
  expect_silent(makeUniqueDistributions(list(Binomial$new(), Binomial$new()))$Binom2)
})

test_that("kurtosis and skew type", {
  expect_equal(exkurtosisType(1), "leptokurtic")
  expect_equal(exkurtosisType(0), "mesokurtic")
  expect_equal(exkurtosisType(-1), "platykurtic")
  expect_equal(skewType(1), "positive skew")
  expect_equal(skewType(0), "no skew")
  expect_equal(skewType(-1), "negative skew")
})
