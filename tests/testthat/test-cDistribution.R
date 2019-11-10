library(testthat)

context("c.Distribution")

test_that("non-distlist",{
  expect_error(c(Binomial$new(), Binomial), "One or more...")
  expect_silent(expect_length(c(Binomial, Binomial$new()), 2))
})

test_that("SDistributions",{
  expect_silent(c(Binomial$new(), Normal$new()))
  expect_equal(getR6Class(c(Binomial$new(), Normal$new())), "VectorDistribution")
  expect_equal(c(Binomial$new(), Normal$new())$short_name, "BinomVecNorm")
})

test_that("VectorDistributions",{
  v1 = VectorDistribution$new(list(Binomial$new(), Normal$new()))
  v2 = VectorDistribution$new(distribution = "Gamma", params  = data.table::data.table(shape = 1:2, rate = 1:2))
  expect_silent(c(v1, v2))
  expect_silent(c(v1, v2, Normal$new(), truncate(Binomial$new(), 2, 6)))
})

test_that("distribution/param VectorDistributions",{
  v1 = VectorDistribution$new(distribution = c("Binomial","Normal"),
                              params = list(list(size = 2), list(mean = 0, var = 2)))
  v2 = VectorDistribution$new(distribution = "Gamma", params  = data.table::data.table(shape = 1:2, rate = 1:2))
  expect_silent(c(v1, v2))
  v3 = c(v1, v2)
  expect_false(v3$distlist)
})
