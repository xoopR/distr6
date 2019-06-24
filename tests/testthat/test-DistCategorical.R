library(testthat)

context("Categorical distribution")

test_that("constructor",{
  expect_error(Categorical$new())
  expect_error(Categorical$new(probs = c(0.1,0.4)))
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
  expect_error(cat$mean())
  expect_error(cat$var())
  expect_error(cat$skewness())
  expect_error(cat$kurtosis())
  expect_error(cat$entropy())
  expect_error(cat$mgf(1:3))
  expect_error(cat$pgf(1:3))
  expect_error(cat$cf(1:3))
  expect_equal(cat$mode(), "T")
  expect_equal(cat$pdf("T"), 4/5)
  expect_equal(cat$pdf(c("Sandwich","Fish")), c(0.1/5,0))
  expect_equal(length(cat$rand(10)),10)
})
