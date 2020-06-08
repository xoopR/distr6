library(testthat)

test_that("constructor", {
  expect_silent(ProductDistribution$new(list(Binomial$new(), Binomial$new(size = 20, prob = 0.6))))
  expect_silent(ProductDistribution$new(list(Binomial$new(), Exponential$new(rate = 1))))
  expect_silent(ProductDistribution$new(
    distribution = "Binomial",
    params = list(
      list(prob = 0.1, size = 2),
      list(prob = 0.75, size = 3)
    )
  ))
  expect_error(ProductDistribution$new(distribution = Dist, params = list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
})

pd <- ProductDistribution$new(
  distribution = "Binomial",
  params = data.table(size = c(40, 5), prob = c(0.2, 0.5))
)

test_that("pdf/cdf", {
  expect_equal(
    pd$pdf(1:2, 8:9),
    c(dbinom(1, 40, 0.2) * dbinom(8, 5, 0.9), dbinom(2, 40, 0.2) * dbinom(9, 5, 0.9))
  )
  expect_equal(
    pd$cdf(1:2, 8:9),
    c(pbinom(1, 40, 0.2) * pbinom(8, 5, 0.9), pbinom(2, 40, 0.2) * pbinom(9, 5, 0.9))
  )
})

test_that("rand", {
  expect_equal(dim(pd$rand(10)), c(10, 2))
})
