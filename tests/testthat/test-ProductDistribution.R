library(testthat)

context("Product Distribution")

test_that("constructor", {
  expect_silent(ProductDistribution$new(list(Binomial$new(), Binomial$new(size = 20, prob = 0.6))))
  expect_silent(ProductDistribution$new(list(Binomial$new(), Exponential$new(rate = 1))))
  expect_silent(ProductDistribution$new(distribution = Binomial, params = list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ProductDistribution$new(distribution = Binomial, params = list(list(prob = 2, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ProductDistribution$new(distribution = Binomial, params = list(list(re = 2, size = 2), list(prob = 0.75, size = 3))))
  expect_error(ProductDistribution$new(distribution = Dist, params = list(list(prob = 0.1, size = 2), list(prob = 0.75, size = 3))))
})

test_that("type/support", {
  expect_equal(ProductDistribution$new(list(Binomial$new(), Binomial$new()))$type$strprint(), setpower(Naturals$new(), 2)$strprint())
  expect_equal(
    ProductDistribution$new(list(Binomial$new(size = 2), Binomial$new(size = 3)))$support$strprint(),
    setproduct(Set$new(0:2), Set$new(0:3))$strprint()
  )
})

test_that("pdf/cdf", {
  expect_equal(
    ProductDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$pdf(4, x2 = 8),
    Binomial$new(size = 40, prob = 0.2)$pdf(4) * Binomial$new(size = 5, prob = 0.9)$pdf(8)
  )
  expect_equal(
    ProductDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$pdf(4, x2 = 3),
    Binomial$new(size = 40, prob = 0.2)$pdf(4) * Binomial$new(size = 5, prob = 0.9)$pdf(3)
  )
  expect_equal(
    ProductDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$cdf(4, x2 = 8),
    Binomial$new(size = 40, prob = 0.2)$cdf(4) * Binomial$new(size = 5, prob = 0.9)$cdf(8)
  )
})

test_that("rand", {
  expect_equal(
    dim(ProductDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$rand(5)),
    c(5, 2)
  )
})

test_that("pdf/cdf - array", {
  a <- ProductDistribution$new(distribution = Binomial, params = list(
    list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$pdf(1, x2 = 2, x3 = 3), Binomial$new(2, 0.1)$pdf(1) * Binomial$new(4, 0.6)$pdf(2) * Binomial$new(6, 0.2)$pdf(3))
  expect_equal(a$cdf(1, x2 = 2, x3 = 3), Binomial$new(2, 0.1)$cdf(1) * Binomial$new(4, 0.6)$cdf(2) * Binomial$new(6, 0.2)$cdf(3))
})

test_that("type/support - array", {
  a <- ProductDistribution$new(distribution = Binomial, params = list(
    list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$type$strprint(), setpower(Naturals$new(), 3)$strprint())
  expect_equal(a$support$strprint(), (Set$new(0:2) * Set$new(0:4) * Set$new(0:6))$strprint())
  expect_error(ProductDistribution$new(distribution = sd, params = list()))
  expect_error(ProductDistribution$new())
  expect_equal(ProductDistribution$new(distribution = Binomial, params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))), a)
  expect_error(ProductDistribution$new(distribution = Binomial, params = c(prob = 1)))
})
