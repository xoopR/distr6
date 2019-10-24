library(testthat)

context("Vector Distribution")

test_that("constructor",{
  expect_silent(VectorDistribution$new(list(Binomial$new(),Binomial$new(size = 20, prob = 0.6))))
  expect_silent(VectorDistribution$new(list(Binomial$new(),Exponential$new(rate=1))))
  expect_silent(VectorDistribution$new(distribution = "WeightedDiscrete", params = list(
    data = data.frame(x = 1, prob = 1))
  ))
  expect_error(VectorDistribution$new(), "Either distlist")
  expect_error(VectorDistribution$new(distribution = "Gerald", params = list()), "Gerald is not")
})

test_that("pdf/cdf/quantile",{
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$pdf(4,8),
               data.table::data.table(Binom1 = dbinom(4,40,0.2), Binom2 = dbinom(8,5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$cdf(2:4,1:3),
               data.table::data.table(Binom1 = pbinom(2:4,40,0.2), Binom2 = pbinom(1:3,5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(0.1,0.2),c(0.3,0.4)),
               data.table::data.table(Binom1 = qbinom(c(0.1,0.2),40,0.2), Binom2 = qbinom(c(0.3,0.4),5,0.9)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(0.1,0.2),c(0.3,0.4),lower.tail = F),
               data.table::data.table(Binom1 = qbinom(c(0.1,0.2),40,0.2,lower.tail = F), Binom2 = qbinom(c(0.3,0.4),5,0.9,lower.tail = F)))
  expect_equal(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$quantile(c(-log(4),-log(5)),c(-log(3),-log(6)),log.p = T),
               data.table::data.table(Binom1 = qbinom(c(-log(4),-log(5)),40,0.2,log.p = T), Binom2 = qbinom(c(-log(3),-log(6)),5,0.9,log.p = T)))
})

test_that("rand",{
  expect_equal(dim(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$rand(5)),
               c(5,2))
  expect_equal(dim(VectorDistribution$new(list(Binomial$new(size = 40, prob = 0.2), Binomial$new(size = 5, prob = 0.9)))$rand(1)),
               c(1,2))
})


test_that("pdf/cdf - array",{
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                        list(prob = 0.2, size = 6)))
  expect_equal(a$pdf(1,x2 = 2,x3 = 3), data.table::data.table(Binom1 = Binomial$new(2,0.1)$pdf(1), Binom2 = Binomial$new(4,0.6)$pdf(2), Binom3 = Binomial$new(6,0.2)$pdf(3)))
  expect_equal(a$cdf(1,x2 = 2,x3 = 3), data.table::data.table(Binom1 = Binomial$new(2,0.1)$cdf(1), Binom2 = Binomial$new(4,0.6)$cdf(2), Binom3 = Binomial$new(6,0.2)$cdf(3)))
})

test_that("type/support",{
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                        list(prob = 0.2, size = 6)))
  expect_equal(a$type()$getSymbol(), Reals$new(dim = 3)$getSymbol())
  expect_equal(a$support()$getSymbol(), Reals$new(dim = 3)$getSymbol())
})

test_that("stats", {
  vecDist <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_equal(vecDist$mean(), data.table::data.table(Binom = 5, Gomp = NaN))
  expect_equal(vecDist$mode(), data.table::data.table(Binom = vecDist[1]$mode(), Gomp = NaN))
  expect_equal(vecDist$variance(), data.table::data.table(Binom = 2.5, Gomp = NaN))
  expect_equal(vecDist$skewness(), data.table::data.table(Binom = 0, Gomp = NaN))
  expect_equal(vecDist$kurtosis(), data.table::data.table(Binom = -0.2, Gomp = NaN))
  expect_equal(vecDist$entropy(), data.table::data.table(Binom = Binomial$new()$entropy(), Gomp = NaN))
  expect_equal(vecDist$mgf(2), data.table::data.table(Binom =  Binomial$new()$mgf(2), Gomp = NaN))
  expect_equal(vecDist$cf(2), data.table::data.table(Binom = Binomial$new()$cf(2), Gomp = NaN+0i))
  expect_equal(vecDist$pgf(2), data.table::data.table(Binom = Binomial$new()$pgf(2), Gomp = NaN))
})

test_that("wrapped models",{
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                      list(prob = 0.2, size = 6)))
  expect_equal(a$wrappedModels("Binom1"), Binomial$new(prob=0.1,size=2))
  expect_equal(a$wrappedModels(), list(Binomial$new(prob=0.1,size=2),Binomial$new(prob=0.6,size=4),
                                       Binomial$new(prob=0.2,size=6)))
  expect_equal(a$wrappedModels("dsdsdsd"), list(Binomial$new(prob=0.1,size=2),Binomial$new(prob=0.6,size=4),
                                       Binomial$new(prob=0.2,size=6)))
  a <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_equal(a$wrappedModels(), list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_equal(a$wrappedModels("Binom"), Binomial$new())
  expect_equal(a$wrappedModels(c("Binom","Gomp")), list(Binom=Binomial$new(prob = 0.5, size = 10),
                                                        Gomp=Gompertz$new()))
})

test_that("parameters",{
  a <- VectorDistribution$new(list(Binomial$new(prob = 0.5, size = 10), Gompertz$new()))
  expect_null(expect_message(a$getParameterValue(1), "Vector Distribution should not"))
  expect_null(expect_message(a$setParameterValue(f), "Vector Distribution should not"))
  expect_equal(expect_message(a$parameters(s), "Vector Distribution should not"), data.table::data.table())
})

test_that("extract",{
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                      list(prob = 0.2, size = 6)))
  expect_equal(a[1], Binomial$new(prob = 0.1, size = 2))
  expect_equal(a[1:2]$wrappedModels(), list(Binomial$new(prob = 0.1, size = 2),
                                                   Binomial$new(prob = 0.6, size = 4)))
  expect_error(a[4], "Index i too large")
  a = VectorDistribution$new(list(Binomial$new(prob = 0.1, size = 2), Binomial$new(prob = 0.6, size = 4),
                                  Binomial$new(prob = 0.2, size = 6)))
  expect_equal(a[1], Binomial$new(prob = 0.1, size = 2))
  expect_equal(a[1:2], VectorDistribution$new(list(Binomial$new(prob = 0.1, size = 2),
                                                   Binomial$new(prob = 0.6, size = 4))))
  expect_error(a[4], "Index i too large")
})

test_that("decorators",{
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                      list(prob = 0.2, size = 6)),
                             decorators = c("CoreStatistics", "ExoticStatistics"))
  expect_equal(a$decorators(), c("CoreStatistics", "ExoticStatistics"))
  expect_equal(a[1]$decorators(), c("CoreStatistics", "ExoticStatistics"))
  a = VectorDistribution$new(distribution = "Binomial", params = list(list(prob = 0.1, size = 2), list(prob = 0.6, size = 4),
                                                                      list(prob = 0.2, size = 6)),
                             decorators = list("CoreStatistics", "ExoticStatistics"))
  expect_equal(a$decorators(), c("CoreStatistics", "ExoticStatistics"))
  a = VectorDistribution$new(list(Binomial$new(prob = 0.1, size = 2), Binomial$new(prob = 0.6, size = 4),
                                  Binomial$new(prob = 0.2, size = 6)),
                             decorators = "ExoticStatistics")
  expect_equal(a$decorators(), "ExoticStatistics")
  expect_equal(a[1]$decorators(), "ExoticStatistics")
})
