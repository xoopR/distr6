bin <- Binomial$new()
norm <- Normal$new()

test_that("non-distlist", {
  expect_error(c(bin, Binomial), "One or more...")
})

test_that("SDistributions", {
  expect_equal_distr(c(bin, bin),
               VectorDistribution$new(distribution = "Binomial",
                                      params = data.table(prob = 0.5, size = c(10, 10))))
  expect_equal(getR6Class(c(bin, norm)), "VectorDistribution")
  expect_equal(c(bin, norm)$short_name, "BinomVecNorm")
})

## FIXME: BROKEN BECAUSE TRAFO RUNNING ON NORMAL MEAN AND GAMMA MEAN
## NEED A WAY TO PULL PREFIX INTO TRAFO WITHOUT COMPROMISING VECTORISATION
test_that("VectorDistributions", {
  v1 <- VectorDistribution$new(list(bin, norm))
  v2 <- VectorDistribution$new(distribution = "Gamma",
                               params = data.table::data.table(shape = 1:2, rate = 1:2))
  expect_silent(c(v1, v2))
  expect_silent(c(v1, v2, norm, truncate(bin, 2, 6)))

  v1 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 1:2))
  v2 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 3:4))
  expect_equal_distr(c(v1, v2)$cdf(5),
      VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 1:4))$cdf(5))

  v2 <- VectorDistribution$new(distribution = "Geometric", params = data.frame(prob = c(0.1, 0.2)))
  expect_equal_distr(c(v1, v2)$pdf(1:10),
      VectorDistribution$new(list(Binomial$new(size = 1), Binomial$new(size = 2),
                                  Geometric$new(prob = 0.1), Geometric$new(prob = 0.2)))$pdf(1:10))
})

test_that("weighteddiscrete vec", {
  v1 <- VectorDistribution$new(
    distribution = "WeightedDiscrete",
    params = list(
      data = data.frame(x = 1, pdf = 1),
      data = data.frame(x = 2, pdf = 1)
    )
  )
  v2 <- VectorDistribution$new(
    distribution = "WeightedDiscrete",
    params = list(
      data = data.frame(x = 3, pdf = 1),
      data = data.frame(x = 4, pdf = 1)
    )
  )
  expect_silent(c(v1, v2))
})

test_that("different lengths", {
  v1 <- VectorDistribution$new(
    distribution = "WeightedDiscrete",
    params = list(
      list(x = 1, pdf = 1)
    )
  )
  v2 <- VectorDistribution$new(
    distribution = "WeightedDiscrete",
    params = list(
      list(x = 3, pdf = 1),
      list(x = 4, pdf = 1)
    )
  )
  expect_silent(c(v1, v2))
})
