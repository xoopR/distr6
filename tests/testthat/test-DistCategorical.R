library(testthat)

context("Categorical distribution")

test_that("autottest",{
  autotest_sdistribution(Categorical,
                         pars = list("Sandwich",4,"T", probs = c(0.1,0.9,4)),
                         traits = list(type = Complex$new(), valueSupport = "discrete", variateForm = "univariate"),
                         support = Set$new("Sandwich",4,"T"),
                         symmetry = "asymmetric",
                         mean = NaN,
                         mode = "T",
                         variance = NaN,
                         skewness = NaN,
                         exkur = NaN,
                         entropy = 2.7081,
                         mgf = NaN,
                         cf = NaN,
                         pgf = NaN
  )
})

test_that("constructor",{
  expect_equal(Categorical$new()$getParameterValue("probs"),1)
  expect_silent(Categorical$new(probs = c(0.1,0.4)))
  expect_silent(Categorical$new("Bapple","Banana",probs = c(0.1,0.4)))
  expect_error(Categorical$new("Bapple","Banana",probs = c(0.1,0.4,0.2)))
})

test_that("parameters", {
  expect_equal(Categorical$new()$getParameterValue("categories"), 3)
  expect_equal(Categorical$new()$getParameterValue("probs"), c(0.1,0.9,4)/5)
})

test_that("properties & traits",{
  expect_equal(Categorical$new()$symmetry, "asymmetric")
})


test_that("statistics",{
  expect_equal(Categorical$new("A","B",probs = c(0.5,0.5))$mode(which = 2), "B")
  expect_equal(cat$pdf("T"), 4/5)
  expect_equal(cat$pdf(c("Sandwich","Fish")), c(0.1/5,0))
  expect_equal(cat$cdf(x1 = c(4,"Fish")), c(1/5,0))
  expect_equal(cat$cdf(x1 = c("Sandwich",4)), c(0.1/5,1/5))
})
