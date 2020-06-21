library(testthat)

test_that("constructor", {
  expect_silent(VectorDistribution$new(list(Binomial$new(), Binomial$new(size = 20, prob = 0.6))))
  expect_silent(VectorDistribution$new(list(Binomial$new(), Exponential$new(rate = 1))))
  expect_silent(VectorDistribution$new(distribution = "WeightedDiscrete", params = list(
    list(x = 1, pdf = 1)
  )))
  expect_error(VectorDistribution$new(), "Either distlist")
  expect_error(VectorDistribution$new(distribution = "Gerald", params = list()), "should be one of")
})

vd <- VectorDistribution$new(
  distribution = "Binomial",
  params = data.table(size = c(40, 5), prob = c(0.2, 0.5))
)

 test_that("pdf/cdf/quantile", {
  expect_equal(vd$pdf(1:2, 3:4), data.table(Binom1 = dbinom(1:2, 40, 0.2),
                                            Binom2 = dbinom(3:4, 5, 0.5)))
  expect_equal(vd$cdf(1:2, 3:4), data.table(Binom1 = pbinom(1:2, 40, 0.2),
                                            Binom2 = pbinom(3:4, 5, 0.5)))
  expect_equal(
    vd$quantile(c(0.1, 0.2), c(0.3, 0.4)),
    data.table(Binom1 = qbinom(c(0.1, 0.2), 40, 0.2), Binom2 = qbinom(c(0.3, 0.4), 5, 0.5))
  )

  a <- VectorDistribution$new(list(
    Binomial$new(prob = 0.2, size = 40),
    Binomial$new(prob = 0.5, size = 5)
  ))
  expect_equal(a$pdf(1:2, 3:4), data.table(Binom1 = dbinom(1:2, 40, 0.2),
                                           Binom2 = dbinom(3:4, 5, 0.5)))
  expect_equal(a$cdf(1:2, 3:4), data.table(Binom1 = pbinom(1:2, 40, 0.2),
                                           Binom2 = pbinom(3:4, 5, 0.5)))
  expect_equal(
    a$quantile(c(0.1, 0.2), c(0.3, 0.4)),
    data.table(Binom1 = qbinom(c(0.1, 0.2), 40, 0.2), Binom2 = qbinom(c(0.3, 0.4), 5, 0.5))
  )
})

test_that("rand", {
  expect_equal(dim(vd$rand(10)), c(10, 2))
})


test_that("pdf/cdf - !distlist", {
  a <- VectorDistribution$new(distribution = "Binomial", params = list(
    list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$pdf(1, x2 = 2, x3 = 3),
               data.table::data.table(Binom1 = Binomial$new(2, 0.1)$pdf(1),
                                      Binom2 = Binomial$new(4, 0.6)$pdf(2),
                                      Binom3 = Binomial$new(6, 0.2)$pdf(3)))
  expect_equal(a$cdf(1, x2 = 2, x3 = 3),
               data.table::data.table(Binom1 = Binomial$new(2, 0.1)$cdf(1),
                                      Binom2 = Binomial$new(4, 0.6)$cdf(2),
                                      Binom3 = Binomial$new(6, 0.2)$cdf(3)))
})
#
# test_that("type/support", {
#   a <- VectorDistribution$new(distribution = "Binomial", params = list(
#     list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
#     list(prob = 0.2, size = 6)
#   ))
#   expect_equal(a$type$strprint(), setpower(Reals$new(), 3)$strprint())
#   expect_equal(a$support$strprint(), setpower(Reals$new(), 3)$strprint())
# })

test_that("stats", {
  vecDist <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_equal(vecDist$mean(), c(Binom = 5, Gomp = NaN))
  expect_equal(vecDist$mode(), c(Binom = vecDist[1]$mode(), Gomp = NaN))
  expect_equal(vecDist$variance(), c(Binom = 2.5, Gomp = NaN))
  expect_equal(vecDist$skewness(), c(Binom = 0, Gomp = NaN))
  expect_equal(vecDist$kurtosis(), c(Binom = -0.2, Gomp = NaN))
  expect_equal(vecDist$entropy(), c(Binom = Binomial$new()$entropy(), Gomp = NaN))
  expect_equal(vecDist$mgf(2), c(Binom = Binomial$new()$mgf(2), Gomp = NaN))
  expect_equal(vecDist$cf(2), c(Binom = Binomial$new()$cf(2), Gomp = NaN + 0i))
  expect_equal(vecDist$pgf(2), c(Binom = Binomial$new()$pgf(2), Gomp = NaN))

  vecDist <- VectorDistribution$new(
    distribution = "Binomial",
    params = data.table(size = 1:2, prob = c(0.1, 0.2))
  )
  expect_equal(vecDist$mean(), c(Binom1 = vecDist[1]$mean(), Binom2 = vecDist[2]$mean()))
  expect_equal(vecDist$mode(), c(Binom1 = vecDist[1]$mode(), Binom2 = vecDist[2]$mode()))
  expect_equal(vecDist$variance(), c(Binom1 = vecDist[1]$variance(),
                                     Binom2 = vecDist[2]$variance()))
  expect_equal(vecDist$skewness(), c(Binom1 = vecDist[1]$skewness(),
                                     Binom2 = vecDist[2]$skewness()))
  expect_equal(vecDist$kurtosis(), c(Binom1 = vecDist[1]$kurtosis(),
                                     Binom2 = vecDist[2]$kurtosis()))
  expect_equal(vecDist$entropy(), c(Binom1 = vecDist[1]$entropy(), Binom2 = vecDist[2]$entropy()))
})

test_that("wrapped models", {
  a <- VectorDistribution$new(distribution = "Binomial", params = list(
    list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$wrappedModels("Binom1"), Binomial$new(prob = 0.1, size = 2))
  expect_equal(a$wrappedModels(), list(
    Binom1 = Binomial$new(prob = 0.1, size = 2),
    Binom2 = Binomial$new(prob = 0.6, size = 4),
    Binom3 = Binomial$new(prob = 0.2, size = 6)
  ))
  a <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_equal(
    a$wrappedModels(),
    list(
      Binom = Binomial$new(prob = 0.5, size = 10),
      Gomp = Gompertz$new()
    )
  )
  expect_equal(a$wrappedModels(c("Binom", "Gomp")), list(
    Binom = Binomial$new(prob = 0.5, size = 10),
    Gomp = Gompertz$new()
  ))
})

# test_that("parameters", {
#   a <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
#   expect_null(expect_message(a$getParameterValue("prob"), "Vector Distribution should not"))
#   expect_null(expect_message(a$setParameterValue(f), "Vector Distribution should not"))
#   expect_equal(expect_message(a$parameters(s), "Vector Distribution should not"),
#   data.table::data.table())
# })

test_that("extract", {
  a <- VectorDistribution$new(distribution = "Binomial", params = list(
    list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a[1], Binomial$new(prob = 0.1, size = 2))
  expect_equal(a[1:2]$wrappedModels(), list(
    Binom1 = Binomial$new(prob = 0.1, size = 2),
    Binom2 = Binomial$new(prob = 0.6, size = 4)
  ))
  expect_error(a[4], "Index i too large")
  a <- VectorDistribution$new(list(
    Binomial$new(prob = 0.1, size = 2), Binomial$new(prob = 0.6, size = 4),
    Binomial$new(prob = 0.2, size = 6)
  ))
  expect_equal(a[1], Binomial$new(prob = 0.1, size = 2))
  expect_equal(a[1:2], VectorDistribution$new(list(
    Binomial$new(prob = 0.1, size = 2),
    Binomial$new(prob = 0.6, size = 4)
  )))
  expect_error(a[4], "Index i too large")
})

test_that("decorators", {
  a <- VectorDistribution$new(
    distribution = "Binomial", params = list(
      list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
      list(prob = 0.2, size = 6)
    ),
    decorators = c("CoreStatistics", "ExoticStatistics")
  )
  expect_equal(a$decorators, c("CoreStatistics", "ExoticStatistics"))
  expect_equal(a[1]$decorators, c("CoreStatistics", "ExoticStatistics"))
  expect_equal(a[1:2]$decorators, c("CoreStatistics", "ExoticStatistics"))

  a <- VectorDistribution$new(list(
    Binomial$new(prob = 0.1, size = 2), Binomial$new(prob = 0.6, size = 4),
    Binomial$new(prob = 0.2, size = 6)
  ),
  decorators = "ExoticStatistics"
  )
  expect_equal(a$decorators, "ExoticStatistics")
  expect_equal(a[1]$decorators, "ExoticStatistics")
  expect_equal(a[1:2]$decorators, "ExoticStatistics")

  expect_equal(as.numeric(a$survival(1)), c(
    1 - Binomial$new(prob = 0.1, size = 2)$cdf(1),
    1 - Binomial$new(prob = 0.6, size = 4)$cdf(1),
    1 - Binomial$new(prob = 0.2, size = 6)$cdf(1)
  ))
})

test_that("shared params", {
  shared_params <- data.table::data.table(prob = 0.2)
  params <- data.table::data.table(size = 1:2)

  vd <- VectorDistribution$new(
    distribution = "Binomial",
    params = params,
    shared_params = shared_params
  )
  expect_equal(vd$parameters()$getParameterValue("prob"), list(Binom1 = 0.2, Binom2 = 0.2))
  expect_equal(vd$parameters()$getParameterValue("size"), list(Binom1 = 1, Binom2 = 2))
})

test_that("shared d/p/q/r", {
  a <- VectorDistribution$new(distribution = "Binomial", params = list(
    list(prob = 0.1, size = 2),
    list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$pdf(1), data.table::data.table(
    Binom1 = dbinom(1, 2, 0.1),
    Binom2 = dbinom(1, 4, 0.6),
    Binom3 = dbinom(1, 6, 0.2)
  ))
  expect_equal(a$cdf(1), data.table::data.table(
    Binom1 = pbinom(1, 2, 0.1),
    Binom2 = pbinom(1, 4, 0.6),
    Binom3 = pbinom(1, 6, 0.2)
  ))
  expect_equal(a$quantile(0.42), data.table::data.table(
    Binom1 = qbinom(0.42, 2, 0.1),
    Binom2 = qbinom(0.42, 4, 0.6),
    Binom3 = qbinom(0.42, 6, 0.2)
  ))
})

test_that("print", {
  a <- VectorDistribution$new(distribution = "Binomial", params = list(
    list(prob = 0.1, size = 2),
    list(prob = 0.6, size = 4),
    list(prob = 0.2, size = 6)
  ))
  expect_equal(a$strprint(), c("Binom1", "Binom2", "Binom3"))
  expect_equal(a$strprint(1), c("Binom1", "...", "Binom3"))
  expect_output(a$print(), "Binom1")
})

test_that("weighted discrete", {
  expect_silent({
    VectorDistribution$new(
      distribution = "WeightedDiscrete",
      params = list(
        list(x = 1:5, pdf = dbinom(1:5, 10, 0.5)),
        list(x = 11:15, cdf = pgeom(1:5, 0.5))
      )
    )
  })

  expect_silent({
    VectorDistribution$new(
      distribution = "WeightedDiscrete",
      params = list(
        list(x = 1:5, pdf = dbinom(1:5, 10, 0.5)),
        list(x = 11:15, cdf = pgeom(1:5, 0.5))
      )
    )
  })
})
