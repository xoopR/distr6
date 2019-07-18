library(testthat)

context("Categorical distribution")

test_that("constructor",{
  expect_equal(Categorical$new()$getParameterValue("probs"),1)
  expect_equal(Categorical$new()$support(),Set$new(1))
  expect_silent(Categorical$new(probs = c(0.1,0.4)))
  expect_silent(Categorical$new("Bapple","Banana",probs = c(0.1,0.4)))
  expect_error(Categorical$new("Bapple","Banana",probs = c(0.1,0.4,0.2)))
})

cat = Categorical$new("Sandwich",4,"T", probs = c(0.1,0.9,4))
test_that("parameters", {
  expect_equal(cat$getParameterValue("categories"), 3)
  expect_equal(cat$getParameterValue("probs"), c(0.1,0.9,4)/5)
})

test_that("properties & traits",{
  expect_equal(cat$valueSupport(), "discrete")
  expect_equal(cat$variateForm(), "univariate")
  expect_equal(cat$symmetry(), "asymmetric")
})


test_that("statistics",{
  expect_equal(cat$mean(), NaN)
  expect_equal(cat$variance(), NaN)
  expect_equal(cat$skewness(), NaN)
  expect_equal(cat$kurtosis(), NaN)
  expect_equal(cat$entropy(), NaN)
  expect_equal(cat$mgf(1:3), NaN)
  expect_equal(cat$pgf(1:3), NaN)
  expect_equal(cat$cf(1:3), NaN)
  expect_equal(cat$mode(), "T")
  expect_equal(cat$pdf("T"), 4/5)
  expect_equal(cat$pdf(c("Sandwich","Fish")), c(0.1/5,0))
  expect_equal(cat$cdf(x1 = c(4,"Fish")), c(1/5,0))
  expect_equal(cat$cdf(x1 = c("Sandwich",4)), c(0.1/5,1/5))
  expect_equal(length(cat$rand(10)),10)
})
