test_that("rep custom", {
  dist = Distribution$new(pdf = function(x) 1/10, name = "Test", type = Set$new(0:10, class = "integer"))
  rep_dist = rep(dist, 3)
  expect_equal(getR6Class(rep_dist), "VectorDistribution")
  expect_equal(rep_dist$pdf(1), data.table(Test1=0.1,Test2=0.1,Test3=0.1))
})

test_that("rep dist/param", {
  expect_equal(rep(Binomial$new(size = 2, prob = 0.1), 10, "mix")$pdf(1:5),
               MixtureDistribution$new(distribution = "Binomial",
                                       params = rep(list(list(size = 2, prob = 0.1)), 10))$pdf(1:5))
})

test_that("rep kernel", {
  expect_silent(rep(Epanechnikov$new(), 5))
  expect_silent(rep(UniformKernel$new(), 2, "prod"))
})
