test_that("dstr", {
  expect_equal(dstr("Norm")$strprint(), "Norm(mean = 0, var = 1)")
  expect_equal(dstr("Exponential")$strprint(), "Exp(rate = 1)")
  expect_equal(dstr("Binomial", size = 5, prob = 0.1)$strprint(),
               "Binom(size = 5, prob = 0.1)")
  expect_equal(dstr("Gamma", decorators = "ExoticStatistics")$decorators, "ExoticStatistics")
  expect_equal(dstr("Gamma", pars = list(shape = 2, rate = 4))$strprint(),
               "Gamma(shape = 2, rate = 4)")
})

test_that("dstrs", {
  v <- dstrs(c("Binom", "Gamma"))
  expect_equal(getR6Class(v), "VectorDistribution")
  expect_equal(v$strprint(), c("Binom", "Gamma"))
  v <- dstrs(c("Binom", "Gamma"), list(list(size = 4), NULL))
  expect_equal(v[1]$strprint(), "Binom(size = 4, prob = 0.5)")
  v <- dstrs(c("Binom", "Gamma"), list(list(size = 4), list(rate = 2, shape = 3)))
  expect_equal(v[1]$strprint(), "Binom(size = 4, prob = 0.5)")
  expect_equal(v[2]$strprint(), "Gamma(shape = 3, rate = 2)")
  v <- dstrs("Binom", data.frame(size = 1:2))
  expect_equal(v[1]$strprint(), "Binom(prob = 0.5, size = 1)")
  expect_equal(v[2]$strprint(), "Binom(prob = 0.5, size = 2)")
})
