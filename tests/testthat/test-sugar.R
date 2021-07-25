test_that("dstr", {
  expect_equal_distr(dstr("Norm"), Normal$new())
  expect_equal_distr(dstr("Exponential"), Exponential$new())
  expect_equal_distr(dstr("Binomial", size = 5, prob = 0.1),
                            Binomial$new(size = 5, prob = 0.1))
  expect_equal(dstr("Gamma", decorators = "ExoticStatistics")$decorators,
               "ExoticStatistics")
  expect_equal_distr(dstr("Gamma", pars = list(shape = 2, rate = 4)),
               Gamma$new(shape = 2, rate = 4))
})

test_that("dstrs", {
  v <- dstrs(c("Binom", "Gamma"))
  expect_equal(getR6Class(v), "VectorDistribution")
  expect_equal(v$strprint(), c("Binom", "Gamma"))
  v <- dstrs(c("Binom", "Gamma"), list(list(size = 4), NULL))
  expect_equal_distr(v[1], Binomial$new(size = 4, prob = 0.5))
  v <- dstrs(c("Binom", "Gamma"), list(list(size = 4), list(rate = 2, shape = 3)))
  expect_equal_distr(v[1], Binomial$new(size = 4, prob = 0.5))
  expect_equal_distr(v[2], Gamma$new(shape = 3, rate = 2))
  v <- dstrs("Binom", data.frame(size = 1:2, prob = 0.5))
  expect_equal_distr(v[1], Binomial$new(prob = 0.5, size = 1))
  expect_equal_distr(v[2], Binomial$new(prob = 0.5, size = 2))
})
