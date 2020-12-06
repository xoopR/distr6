test_that("constructor", {
  expect_silent(VectorDistribution$new(list(Binomial$new(), Binomial$new(size = 20, prob = 0.6))))
  expect_silent(VectorDistribution$new(list(Binomial$new(), Exponential$new(rate = 1))))
  expect_silent(VectorDistribution$new(distribution = "WeightedDiscrete", params = list(
    list(x = 1, pdf = 1)
  )))
  expect_error(VectorDistribution$new(), "Either distlist")
  expect_error(VectorDistribution$new(distribution = "Gerald", params = list()), "should be one of")
})

test_that("special cases", {
  expect_error(VectorDistribution$new(distribution = "Empirical", params = list()),
               "use `distlist`")
  expect_error(VectorDistribution$new(distribution = "Geom", params = list(trials = FALSE)),
               "shared_params")
  expect_error(VectorDistribution$new(distribution = "Negative", params = list(form = "tbf")),
               "shared_params")
})

vd <- VectorDistribution$new(
  distribution = "Binomial",
  params = data.table(size = c(40, 5), prob = c(0.2, 0.5))
)

test_that("errors", {
  expect_error(vd$pdf(data = matrix(1, ncol = 3)), "Expected data with")
  expect_error(vd$cdf(data = matrix(1, ncol = 3)), "Expected data with")
  expect_error(vd$quantile(data = matrix(1, ncol = 3)), "Expected data with")
  expect_error(vd$pdf(1, 2, 3, 4), "Expected data with")
  expect_error(vd$cdf(1, 2, 3, 4), "Expected data with")
  expect_error(vd$quantile(1, 2, 3, 4), "Expected data with")
})

test_that("one col", {
  expect_equal(vd$pdf(data = matrix(1:3, ncol = 1)),
               data.table(Binom1 = dbinom(1:3, 40, 0.2),
                          Binom2 = dbinom(1:3, 5, 0.5)))
  expect_equal(vd$cdf(data = matrix(1:3, ncol = 1)),
               data.table(Binom1 = pbinom(1:3, 40, 0.2),
                          Binom2 = pbinom(1:3, 5, 0.5)))
  expect_equal(vd$quantile(data = matrix(c(0.1, 0.2), ncol = 1)),
               data.table(Binom1 = qbinom(c(0.1, 0.2), 40, 0.2),
                          Binom2 = qbinom(c(0.1, 0.2), 5, 0.5)))
})

test_that("one row", {
  expect_equal(as.numeric(unlist(vd$pdf(data = matrix(1:2, nrow = 1)))),
               dbinom(1:2, size = c(40, 5), prob = c(0.2, 0.5)))
  expect_equal(as.numeric(unlist(vd$cdf(data = matrix(1:2, nrow = 1)))),
               pbinom(1:2, size = c(40, 5), prob = c(0.2, 0.5)))
  expect_equal(as.numeric(unlist(vd$quantile(data = matrix(c(0.1, 0.2), nrow = 1)))),
               qbinom(c(0.1, 0.2), size = c(40, 5), prob = c(0.2, 0.5)))

  avd <- VectorDistribution$new(
    distribution = "Arcsine",
    params = data.table(lower = c(-2, -3), upper = 4:5)
  )

  expect_equal(as.numeric(unlist(avd$pdf(data = matrix(1:2, nrow = 1)))),
              c(Arcsine$new(-2, 4)$pdf(1), Arcsine$new(-3, 5)$pdf(2)))
  expect_equal(as.numeric(unlist(avd$cdf(data = matrix(1:2, nrow = 1)))),
               c(Arcsine$new(-2, 4)$cdf(1), Arcsine$new(-3, 5)$cdf(2)))
  expect_equal(as.numeric(unlist(avd$quantile(data = matrix(c(0.1, 0.2), nrow = 1)))),
               c(Arcsine$new(-2, 4)$quantile(0.1), Arcsine$new(-3, 5)$quantile(0.2)))
})

 test_that("pdf/cdf/quantile/rand", {
  expect_equal(vd$pdf(1:10), vd$pdf(1:10, 1:10))
  expect_equal(vd$cdf(1:10), vd$cdf(1:10, 1:10))
  expect_equal(vd$quantile(1:10), vd$quantile(1:10, 1:10))

  expect_equal(vd$quantile(c(0.2, 0.4)), vd$quantile(c(0.2, 0.4)))
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

  expect_equal(dim(vd$rand(10)), c(10, 2))

  expect_equal(a$pdf(1:10), a$pdf(1:10, 1:10))
  expect_equal(a$cdf(1:10), a$cdf(1:10, 1:10))
  expect_equal(a$quantile(c(0.1, 0.2)), a$quantile(c(0.1, 0.2), c(0.1, 0.2)))
})

test_that("distlist rand", {
  vd <- VectorDistribution$new(list(Binomial$new(), Geometric$new()))
  checkmate::expect_data_table(vd$rand(1), nrows = 1, ncols = 2)
  expect_equal(colnames(vd$rand(1)), c("Binom", "Geom"))
  checkmate::expect_data_table(vd$rand(3), nrows = 3, ncols = 2)
  expect_equal(colnames(vd$rand(3)), c("Binom", "Geom"))
})


vd <- VectorDistribution$new(
  distribution = "Binomial",
  params = data.table(size = c(40, 5), prob = c(0.2, 0.5))
)

test_that("generators", {
  b1 <- Binomial$new(size = 40, prob = 0.2)
  b2 <- Binomial$new(size = 5, prob = 0.5)

  expect_warning(expect_equal(vd$mgf(1), c(Binom1 = b1$mgf(1), Binom2 = b2$mgf(1))), "vectorised")
  expect_warning(expect_equal(vd$mgf(1:2), data.table(Binom1 = b1$mgf(1:2), Binom2 = b2$mgf(1:2))))

  expect_warning(expect_equal(vd$pgf(1), c(Binom1 = b1$pgf(1), Binom2 = b2$pgf(1))), "vectorised")
  expect_warning(expect_equal(vd$pgf(1:2), data.table(Binom1 = b1$pgf(1:2), Binom2 = b2$pgf(1:2))))

  expect_warning(expect_equal(vd$cf(1), c(Binom1 = b1$cf(1), Binom2 = b2$cf(1))), "vectorised")
  expect_warning(expect_equal(vd$cf(1:2), data.table(Binom1 = b1$cf(1:2), Binom2 = b2$cf(1:2))))
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
  expect_equal(a$wrappedModels("Binom1")$cdf(1:10), Binomial$new(prob = 0.1, size = 2)$cdf(1:10))
  expect_equal(a$wrappedModels()[1][[1]]$cdf(1:10), Binomial$new(prob = 0.1, size = 2)$cdf(1:10))
  expect_equal(a$wrappedModels()[2][[1]]$cdf(1:10), Binomial$new(prob = 0.6, size = 4)$cdf(1:10))
  expect_equal(a$wrappedModels()[3][[1]]$cdf(1:10), Binomial$new(prob = 0.2, size = 6)$cdf(1:10))

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

  mix <- MixtureDistribution$new(list(Binomial$new(), Normal$new()))
  expect_equal(mix$wrappedModels("Binom"), Binomial$new())
  expect_equal(mix$wrappedModels(), list(Binom = Binomial$new(), Norm = Normal$new()))
  expect_error(mix$wrappedModels("sdsd"), "No distribution called")
  expect_equal(mix$wrappedModels(c("Binom", "Norm")), list(Binom = Binomial$new(),
                                                           Norm = Normal$new()))
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
  expect_equal(a[1]$pdf(1:10), Binomial$new(prob = 0.1, size = 2)$pdf(1:10))
  expect_equal(names(a[1:2]$wrappedModels()), c("Binom1", "Binom2"))
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

test_that("multivariate", {
  vd <- VectorDistribution$new(
   distribution = "Multinomial",
   params = list(
   list(size = 5, probs = c(0.1, 0.9)),
   list(size = 8, probs = c(0.3, 0.7))
   ))

  expect_equal(vd$pdf(1, 4), vd$pdf(data = array(c(1, 4), dim = c(1, 2, 2))))

  expect_error(vd$pdf(data = matrix(1), "multivariate"))
  expect_error(vd$pdf(1, "multivariate"))
  expect_error(vd$cdf(data = matrix(1), "multivariate"))
  expect_error(vd$cdf(1, "multivariate"))
  expect_error(vd$quantile(1, "not possible"))

  expect_equal(vd$pdf(data = array(c(1, 4, 2, 6), dim = c(1, 2, 2))),
               data.table(Multinom1 = Multinomial$new(size = 5, probs = c(0.1, 0.9))$pdf(1, 4),
                          Multinom2 = Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(2, 6)))

  vd <- VectorDistribution$new(list(Multinomial$new(size = 5, probs = c(0.1, 0.9)),
                                    Multinomial$new(size = 8, probs = c(0.3, 0.7))))


  expect_equal(vd$pdf(data = array(c(1, 4, 2, 6), dim = c(1, 2, 2))),
               data.table(Multinom1 = Multinomial$new(size = 5, probs = c(0.1, 0.9))$pdf(1, 4),
                          Multinom2 = Multinomial$new(size = 8, probs = c(0.3, 0.7))$pdf(2, 6)))

  expect_equal(vd$pdf(1, 4), vd$pdf(data = array(c(1, 4), dim = c(1, 2, 2))))

  e1 <- EmpiricalMV$new(data.frame(1:5, 1:5))
  e2 <- EmpiricalMV$new(data.frame(11:15, 11:15))
  vd <- VectorDistribution$new(list(e1, e2))

  expect_equal(vd$pdf(data = array(c(1, 4, 12, 16), dim = c(1, 2, 2))),
               data.table(EmpMV1 = e1$pdf(3, 4),
                          EmpMV2 = e2$pdf(12, 16)))

  expect_equal(vd$cdf(data = matrix(c(1, 4), nrow = 1, ncol = 2)),
               data.table(EmpMV1 = e1$cdf(1, 4),
                          EmpMV2 = e2$cdf(1, 4)))

  expect_equal(vd$cdf(data = array(c(1, 4, 12, 16), dim = c(1, 2, 2))),
               data.table(EmpMV1 = e1$cdf(1, 4),
                          EmpMV2 = e2$cdf(12, 16)))

  rand <- expect_silent(vd$rand(1))
  expect_is(rand, "array")
  expect_equal(dim(rand), c(1, 2, 2))
  rand <- expect_silent(vd$rand(3))
  expect_is(rand, "array")
  expect_equal(dim(rand), c(3, 2, 2))
})

test_that("median", {
  vd <- VectorDistribution$new(distribution = "Geometric", params = data.table(prob = c(0.1, 0.2)))
  expect_equal(vd$median(), data.table(Geom1 = Geometric$new(0.1)$median(),
                                       Geom2 = Geometric$new(0.2)$median()))
})

test_that("custom single row", {
  v <- VectorDistribution$new(distribution = "Arcsine", params = data.frame(upper = (1:10) + 10))
  p1 <- v$pdf(data = matrix(1:10, nrow = 1))
  p2 <- sapply((1:10) + 10, function(x) Arcsine$new(upper = x)$pdf(x - 10))
  expect_equal(as.numeric(unlist(p1)), p2)
})

test_that("vecdist constructor", {
  v <- VectorDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))
  m <- MixtureDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))
  p <- ProductDistribution$new(distribution = "Binom", params = data.frame(size = 1:2))

  expect_equal(as.VectorDistribution(p)$pdf(1:10), v$pdf(1:10))
  expect_equal(as.VectorDistribution(m)$cdf(1:10), v$cdf(1:10))
})
