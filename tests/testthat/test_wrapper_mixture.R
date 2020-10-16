library(testthat)

test_that("check weights", {
  expect_equal(
    MixtureDistribution$new(list(Exponential$new(), Normal$new()))$getParameterValue("mix_weights"),
    "uniform"
  )
  expect_error(
    MixtureDistribution$new(list(Exponential$new(), Normal$new()), weights = "sdsd"),
    "either be a"
  )

  expect_equal(
    MixtureDistribution$new(list(Binomial$new(), Exponential$new(), Normal$new()),
      weights = c(0.1, 0.6, 0.3)
    )$getParameterValue("mix_weights"),
    c(0.1, 0.6, 0.3)
  )
})

M <- MixtureDistribution$new(list(Binomial$new(), Exponential$new(), Normal$new()),
                             weights = c(0.1, 0.6, 0.3), name = "A", short_name = "a")
M2 <- MixtureDistribution$new(list(Binomial$new(), Exponential$new(), Normal$new()))

test_that("update weights", {
  expect_equal(M$setParameterValue(mix_weights = c(1, 2, 3))$getParameterValue("mix_weights"),
               (1:3) / sum(1:3))
  expect_silent(M$setParameterValue(mix_weights = c(0.1, 0.6, 0.3)))
})

test_that("check pdf", {
  expect_equal(
    MixtureDistribution$new(list(Binomial$new(), Exponential$new()))$pdf(1:2),
    c(
      mean(c(Binomial$new()$pdf(1), Exponential$new()$pdf(1))),
      mean(c(Binomial$new()$pdf(2), Exponential$new()$pdf(2)))
    )
  )
  expect_equal(M$pdf(1), Binomial$new()$pdf(1) * 0.1 + Exponential$new()$pdf(1) * 0.6 +
                 Normal$new()$pdf(1) * 0.3)
  expect_equal(M$pdf(1:2), c(
    Binomial$new()$pdf(1) * 0.1 + Exponential$new()$pdf(1) * 0.6 + Normal$new()$pdf(1) * 0.3,
    Binomial$new()$pdf(2) * 0.1 + Exponential$new()$pdf(2) * 0.6 + Normal$new()$pdf(2) * 0.3
  ))

  expect_equal(M2$pdf(1), Binomial$new()$pdf(1) / 3 + Exponential$new()$pdf(1) / 3 +
                 Normal$new()$pdf(1) / 3)
})

test_that("check cdf", {
  expect_equal(
    MixtureDistribution$new(list(Binomial$new(), Exponential$new()))$cdf(1:2),
    c(
      mean(c(Binomial$new()$cdf(1), Exponential$new()$cdf(1))),
      mean(c(Binomial$new()$cdf(2), Exponential$new()$cdf(2)))
    )
  )
  expect_equal(M$cdf(1), Binomial$new()$cdf(1) * 0.1 + Exponential$new()$cdf(1) * 0.6 +
                 Normal$new()$cdf(1) * 0.3)
  expect_equal(M$cdf(1:2), c(
    Binomial$new()$cdf(1) * 0.1 + Exponential$new()$cdf(1) * 0.6 + Normal$new()$cdf(1) * 0.3,
    Binomial$new()$cdf(2) * 0.1 + Exponential$new()$cdf(2) * 0.6 + Normal$new()$cdf(2) * 0.3
  ))

  expect_equal(M2$cdf(1), Binomial$new()$cdf(1) / 3 + Exponential$new()$cdf(1) / 3 +
                 Normal$new()$cdf(1) / 3)
})

test_that("quantile", {
  expect_error(M$quantile(0), "unavailable")
})

test_that("check rand", {
  expect_equal(length(M$rand(10)), 10)
  expect_equal(length(M$rand(1)), 1)
  expect_equal(length(MixtureDistribution$new(list(Exponential$new(), Normal$new()))$rand(2)), 2)
  expect_equal(dim(MixtureDistribution$new(distribution = "Multinomial",
                          params = list(list(size = 4, probs = c(0.1, 0.2)),
                                        list(size = 5, probs = c(0.1, 0.2))))$rand(2)),
               c(2, 2))
})

test_that("strprint", {
  expect_equal(M$strprint(), "Binom wX Exp wX Norm")
})

test_that("multivariate", {
  md <- MixtureDistribution$new(
    distribution = "Multinomial",
    params = list(
      list(size = 8, probs = c(0.1, 0.9)),
      list(size = 8, probs = c(0.3, 0.7))
    ))

  expect_equal(md$pdf(2, 6),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(2, 6) / 2 +
               Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(2, 6) / 2)

  expect_equal(md$pdf(c(1, 2), c(7, 6)),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(c(1, 2), c(7, 6)) / 2 +
                 Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(c(1, 2), c(7, 6)) / 2)

  md <- MixtureDistribution$new(
    distribution = "Multinomial",
    params = list(
      list(size = 8, probs = c(0.1, 0.9)),
      list(size = 8, probs = c(0.3, 0.7))
    ),
    weights = c(0.1, 0.9))

  expect_equal(md$pdf(2, 6),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(2, 6) * 0.1 +
                 Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(2, 6) * 0.9)

  expect_equal(md$pdf(c(1, 2), c(7, 6)),
               Multinomial$new(size = 8, probs = c(0.1, 0.9))$pdf(c(1, 2), c(7, 6)) * 0.1 +
                 Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(c(1, 2), c(7, 6)) * 0.9)
})

test_that("vecdist constructor", {
  v <- VectorDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))
  m <- MixtureDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))
  p <- ProductDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))

  expect_equal_distribution(as.MixtureDistribution(v), m)
  expect_equal_distribution(as.MixtureDistribution(p), m)

  v <- VectorDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))
  m <- MixtureDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))
  p <- ProductDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2)))

  expect_equal_distribution(as.MixtureDistribution(v), m)
  expect_equal_distribution(as.MixtureDistribution(p), m)
})
