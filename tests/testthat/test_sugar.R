test_that("dstr", {
  expect_equal(dstr("Norm")$strprint(), "Norm(mean = 0, var = 1, sd = 1, prec = 1)")
  expect_equal(dstr("Exponential")$strprint(), "Exp(rate = 1, scale = 1)")
  expect_equal(dstr("Binomial", size = 5, prob = 0.1)$strprint(),
               "Binom(prob = 0.1, qprob = 0.9, size = 5)")
  expect_equal(dstr("Gamma", decorators = "ExoticStatistics")$decorators, "ExoticStatistics")
  expect_equal(dstr("Gamma", pars = list(shape = 2, rate = 4))$strprint(),
               "Gamma(shape = 2, rate = 4, scale = 0.25, mean = 0.5)")
})

test_that("dstrs", {
  v = dstrs(c("Binom", "Gamma"))
  expect_equal(getR6Class(v), "VectorDistribution")
  expect_equal(v$strprint(), c("Binom", "Gamma"))
  v = dstrs(c("Binom", "Gamma"), list(list(size = 4), NULL))
  expect_equal(v[1]$strprint(), "Binom(prob = 0.5, qprob = 0.5, size = 4)")
  v = dstrs(c("Binom", "Gamma"), list(list(size = 4), list(rate = 2, shape = 3)))
  expect_equal(v[1]$strprint(), "Binom(prob = 0.5, qprob = 0.5, size = 4)")
  expect_equal(v[2]$strprint(), "Gamma(shape = 3, rate = 2, scale = 0.5, mean = 1.5)")
})
