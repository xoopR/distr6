library(testthat)

test_that("constructor", {
  expect_silent(
    ProductDistribution$new(list(Binomial$new(), Binomial$new(size = 20, prob = 0.6)))
  )
  p <- Binomial$new() * Binomial$new(size = 20, prob = 0.6)
  expect_equal(getR6Class(p), "ProductDistribution")
  expect_equal(p$strprint(), "Binom1 X Binom2")
  expect_equal(p[1]$strprint(), "Binom(prob = 0.5, size = 10)")

  expect_silent(ProductDistribution$new(list(Binomial$new(), Exponential$new(rate = 1)),
                                        name = "A", short_name = "a"))
  expect_silent(ProductDistribution$new(
    distribution = "Binomial",
    params = list(
      list(prob = 0.1, size = 2),
      list(prob = 0.75, size = 3)
    )
  ))
  expect_error(ProductDistribution$new(distribution = Dist,
                                       params = list(list(prob = 0.1, size = 2),
                                                     list(prob = 0.75, size = 3))))
})

pd <- ProductDistribution$new(
  distribution = "Binomial",
  params = data.table(size = c(40, 5), prob = c(0.2, 0.5))
)

test_that("pdf/cdf/quantile", {
  expect_equal(
    pd$pdf(1:2, 3:4),
    c(dbinom(1, 40, 0.2) * dbinom(3, 5, 0.5), dbinom(2, 40, 0.2) * dbinom(4, 5, 0.5))
  )
  expect_equal(
    pd$pdf(2),
    dbinom(2, 40, 0.2) * dbinom(2, 5, 0.5)
  )
  expect_equal(
    pd$cdf(1:2, 3:4),
    c(pbinom(1, 40, 0.2) * pbinom(3, 5, 0.5), pbinom(2, 40, 0.2) * pbinom(4, 5, 0.5))
  )

  expect_error(pd$quantile(0), "unavailable")
})

test_that("rand", {
  expect_equal(dim(pd$rand(10)), c(10, 2))
})

test_that("strprint", {
  expect_equal(pd$strprint(), "Binom1 X Binom2")
})

test_that("multivariate", {
  pd <- ProductDistribution$new(
    distribution = "Multinomial",
    params = list(
      list(size = 8, probs = c(0.1, 0.9)),
      list(size = 8, probs = c(0.3, 0.7))
    ))

  expect_equal(pd$pdf(2, 6),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(2, 6) *
                 Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(2, 6))

  expect_equal(pd$pdf(c(1, 2), c(7, 6)),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(c(1, 2), c(7, 6)) *
                 Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(c(1, 2), c(7, 6)))
})

test_that("vecdist constructor", {
  params <- data.frame(size = 1:2, prob = 0.5)
  v <- VectorDistribution$new(distribution = "Binom", params = params)
  m <- MixtureDistribution$new(distribution = "Binom", params = params)
  p <- ProductDistribution$new(distribution = "Binom", params = params)

  pv <- as.ProductDistribution(v)
  mv <- as.ProductDistribution(m)

  expect_equal(pv$cdf(1:10), p$cdf(1:10))
  expect_equal(mv$cdf(1:10), p$cdf(1:10))

  v <- VectorDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))
  m <- MixtureDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))
  p <- ProductDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))

  pv <- as.ProductDistribution(v)
  mv <- as.ProductDistribution(m)

  expect_equal_distr(pv, p)
  expect_equal_distr(mv, p)
})
