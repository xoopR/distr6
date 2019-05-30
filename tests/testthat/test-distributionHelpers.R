library(testthat)

context("Distribution helpers")

test_that("lists",{
  expect_silent(listDistributions())
  expect_silent(listDistributions(simplify = T))
  expect_silent(listDistributions(traits = list(VariateForm = "univariate")))
  expect_silent(listDistributions(traits = list(variateForm = "univariate")))
  expect_silent(listDistributions(traits = list(variateform = "univariate")))
  expect_silent(listDecorators())
  expect_silent(listDecorators(T))
  expect_silent(listSpecialSets())
  expect_silent(listSpecialSets(F))
})

test_that("unique distributions",{
  expect_silent(makeUniqueDistributions(list(Binomial$new(),Binomial$new()))$Binom1)
  expect_silent(makeUniqueDistributions(list(Binomial$new(),Binomial$new()))$Binom2)
})

test_that("kurtosis and skew type",{
  expect_equal(exkurtosisType(1), "leptokurtic")
  expect_equal(exkurtosisType(0), "mesokurtic")
  expect_equal(exkurtosisType(-1), "platykurtic")
  expect_equal(skewType(1), "positive skew")
  expect_equal(skewType(0), "no skew")
  expect_equal(skewType(-1), "negative skew")

})
