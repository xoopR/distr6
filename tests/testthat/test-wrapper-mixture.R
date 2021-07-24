library(testthat)

exp <- Exponential$new()
norm <- Normal$new()
bin <- Binomial$new()

test_that("check weights", {
  expect_equal(
    MixtureDistribution$new(list(exp, norm))$
    getParameterValue("mix__weights"),
    "uniform"
  )
  expect_error(
    MixtureDistribution$new(list(exp, norm), weights = "sdsd"),
    "either be a"
  )

  expect_equal(
    MixtureDistribution$new(list(bin, exp, norm),
      weights = c(0.1, 0.6, 0.3)
    )$getParameterValue("mix__weights"),
    c(0.1, 0.6, 0.3)
  )
})

M <- MixtureDistribution$new(list(bin, exp, norm),
                             weights = c(0.1, 0.6, 0.3), name = "A", short_name = "a")
M2 <- MixtureDistribution$new(list(bin, exp, norm))

test_that("update weights", {
  expect_equal(M$setParameterValue(mix__weights = c(1, 2, 3))$getParameterValue("mix__weights"),
               (1:3) / sum(1:3))
  expect_silent(M$setParameterValue(mix__weights = c(0.1, 0.6, 0.3)))
})

test_that("check pdf", {
  expect_equal(
    MixtureDistribution$new(list(bin, exp))$pdf(1:2),
    c(
      mean(c(bin$pdf(1), exp$pdf(1))),
      mean(c(bin$pdf(2), exp$pdf(2)))
    )
  )
  expect_equal(M$pdf(1), bin$pdf(1) * 0.1 + exp$pdf(1) * 0.6 +
                 norm$pdf(1) * 0.3)
  expect_equal(M$pdf(1:2), c(
    bin$pdf(1) * 0.1 + exp$pdf(1) * 0.6 + norm$pdf(1) * 0.3,
    bin$pdf(2) * 0.1 + exp$pdf(2) * 0.6 + norm$pdf(2) * 0.3
  ))

  expect_equal(M2$pdf(1), bin$pdf(1) / 3 + exp$pdf(1) / 3 +
                 norm$pdf(1) / 3)
})

test_that("check cdf", {
  expect_equal(
    MixtureDistribution$new(list(bin, exp))$cdf(1:2),
    c(
      mean(c(bin$cdf(1), exp$cdf(1))),
      mean(c(bin$cdf(2), exp$cdf(2)))
    )
  )
  expect_equal(M$cdf(1), bin$cdf(1) * 0.1 + exp$cdf(1) * 0.6 +
                 norm$cdf(1) * 0.3)
  expect_equal(M$cdf(1:2), c(
    bin$cdf(1) * 0.1 + exp$cdf(1) * 0.6 + norm$cdf(1) * 0.3,
    bin$cdf(2) * 0.1 + exp$cdf(2) * 0.6 + norm$cdf(2) * 0.3
  ))

  expect_equal(M2$cdf(1), bin$cdf(1) / 3 + exp$cdf(1) / 3 +
                 norm$cdf(1) / 3)
})

test_that("quantile", {
  expect_error(M$quantile(0), "unavailable")
})

test_that("check rand", {
  expect_equal(length(M$rand(10)), 10)
  expect_equal(length(M$rand(1)), 1)
  expect_equal(length(MixtureDistribution$new(list(exp, norm))$rand(2)), 2)
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
  params <- data.frame(size = 1:2, prob = 0.5)
  v <- VectorDistribution$new(distribution = "Binom", params = params)
  m <- MixtureDistribution$new(distribution = "Binom", params = params)
  p <- ProductDistribution$new(distribution = "Binom", params = params)

  expect_equal_distribution(as.MixtureDistribution(v), m)
  expect_equal_distribution(as.MixtureDistribution(p), m)

  b1 <- Binomial$new(size = 1)
  b2 <- Binomial$new(size = 2)

  v <- VectorDistribution$new(list(b1, b2))
  m <- MixtureDistribution$new(list(b1, b2))
  p <- ProductDistribution$new(list(b1, b2))

  expect_equal_distribution(as.MixtureDistribution(v), m)
  expect_equal_distribution(as.MixtureDistribution(p), m)
})
