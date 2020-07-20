v1 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 1:2))
v2 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 3:4))
v3 <- VectorDistribution$new(distribution = "Binomial", params = data.frame(size = 3:5))
v4 <- VectorDistribution$new(distribution = "Geometric", params = data.frame(prob = c(0.1, 0.2)))
v5 <- VectorDistribution$new(list(Binomial$new(prob = 0.1), Binomial$new(size = 20)))
v6 <- VectorDistribution$new(list(Binomial$new(), Geometric$new()))

test_that("errors", {
  expect_error(mixturiseVector(list(v1, v3)), "length")
  expect_error(mixturiseVector(list(v1, v4)), "same type")
  expect_error(mixturiseVector(list(v6, v1)), "one class")
})

test_that("silent", {
  expect_silent({mv1 <- mixturiseVector(list(v1, v2))})
  mv2 <- VectorDistribution$new(list(
      MixtureDistribution$new(distribution = "Binomial", params = data.frame(size = c(1, 3))),
      MixtureDistribution$new(distribution = "Binomial", params = data.frame(size = c(2, 4)))
    ))
  expect_rounded_equal(mv1$cdf(1:10), mv2$cdf(1:10), 6)

  expect_silent({mv1 <- mixturiseVector(list(v1, v2, v5), weights = 1:3)})
  expect_equal(as.numeric(mv1$pdf(1, 2)),
               c(
                 dbinom(1, 1, 0.5) / 6 + dbinom(1, 3, 0.5) / 3 + dbinom(1, 10, 0.1) / 2,
                 dbinom(2, 2, 0.5) / 6 + dbinom(2, 4, 0.5) / 3 + dbinom(2, 20, 0.5) / 2
               ))
})
