library(testthat)

context("Binomial distribution")

test_that("autottest",{
  autotest_sdistribution(Binomial,
                         pars = list(prob = 0.5, size = 10),
                         traits = list(type = Naturals$new(), valueSupport = "discrete", variateForm = "univariate"),
                         support = Set$new(0:10, class = "integer"),
                         symmetry = "symmetric",
                         mean = 5,
                         mode = 5,
                         variance = 2.5,
                         skewness = 0,
                         exkur = -0.2,
                         entropy = 2.7081,
                         mgf = 493.3136,
                         cf = 0.0769-0.2598i,
                         pgf = 1,
                         pdf = dbinom(1:3, size = 10, prob = 0.5),
                         cdf = pbinom(1:3, size = 10, prob = 0.5)
  )
})

test_that("manual test - alternate constructor", {
  expect_silent(Binomial$new(qprob = 0.2))
  expect_equal(Binomial$new(prob = 0.2)$getParameterValue("qprob"), 0.8)
  expect_equal(Binomial$new(qprob = 0.2)$getParameterValue("prob"), 0.8)
})

test_that("manual test - symmetry",{
  expect_equal(Binomial$new(prob = 0.1)$symmetry, "asymmetric")
})
