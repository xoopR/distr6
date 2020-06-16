library(testthat)

test_that("initialize", {
  p = Binomial$new()$parameters()
  expect_silent(ParameterSetCollection$new(lst = list(Binom1 = p, Binom2 = p)))
  pc = ParameterSetCollection$new(Binom1 = p, Binom2 = p)
  expect_equal(getR6Class(pc), "ParameterSetCollection")
  expect_equal(class(pc$.__enclos_env__$private$.parametersets), "list")
  expect_equal(length(pc$.__enclos_env__$private$.parametersets), 2)
  expect_equal(names(pc$.__enclos_env__$private$.parametersets), c("Binom1", "Binom2"))
})

test_that("getters", {
  pc = ParameterSetCollection$new(
    Geom = Geometric$new()$parameters(),
    Binom = Binomial$new()$parameters()
  )
  expect_equal(pc$getParameterValue("Binom_prob"), 0.5)
  expect_equal(pc$getParameterValue("Geom_prob"), 0.5)
  expect_equal(pc$getParameterSupport("Binom_prob"),
               Binomial$new()$parameters()$getParameterSupport("prob"))
})

test_that("setters", {
  pc = ParameterSetCollection$new(
    Geom = Geometric$new()$parameters(),
    Binom = Binomial$new()$parameters()
  )
  expect_equal(pc$getParameterValue("Binom_prob"), 0.5)
  expect_silent(pc$setParameterValue(Binom_prob = 1))
  expect_equal(pc$getParameterValue("Binom_prob"), 1)
  expect_equal(pc$getParameterValue("Binom_qprob"), 0)
})
