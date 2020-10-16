library(testthat)

context("distrSimulate")

test_that("no pars", {
  expect_silent(distrSimulate())
})

test_that("seed", {
  expect_silent(distrSimulate(seed = 5))
})

test_that("abbreviations", {
  expect_equal(distrSimulate(1, distribution = "No", simplify = F)$Distribution$name, "Normal")
  expect_equal(distrSimulate(1, distribution = "Ga", simplify = F)$Distribution$name, "Gamma")
  expect_equal(distrSimulate(1, distribution = "Un", simplify = F)$Distribution$name, "Uniform")
})

test_that("n", {
  expect_equal(distrSimulate(-1), numeric(0))
  expect_equal(distrSimulate(0), numeric(0))
  expect_equal(length(distrSimulate(5)), 5)
  expect_equal(length(distrSimulate(1:3)), 3)
})

test_that("parameters", {
  expect_equal(
    distrSimulate(1, pars = list(mean = 2), simplify = F)$Distribution$getParameterValue("mean"),
    2
  )
})

test_that("simplify", {
  expect_equal(class(distrSimulate(simplify = F)), "list")
  expect_equal(length(distrSimulate(simplify = F)), 2)
  expect_equal(class(distrSimulate(simplify = T)), "numeric")
  expect_equal(length(distrSimulate(simplify = T)), 100)
})
