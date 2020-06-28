library(testthat)

test_that("valueSupport/variateForm", {
  expect_error(plot(MixtureDistribution$new(list(Binomial$new(), Normal$new()))), "Plotting of")
})

test_that("VectorDistribution", {
  vd <- VectorDistribution$new(list(Normal$new(), Normal$new(mean = 2)))
  expect_silent(plot(vd))
  expect_silent(plot(vd, col = c("blue", "orange")))
  expect_error(plot(vd, fun = "pfdskjfndsf"))
  vd <- VectorDistribution$new(rep(list(Normal$new()), 12))
  expect_error(plot(vd, fun = "pfdskjfndsf"))
})

test_that("args", {
  v <- VectorDistribution$new(list(Normal$new(mean = 0), Normal$new(mean = 1)))
  expect_silent(plot(v))
  expect_silent(plot(v, ind = 1))
  expect_silent(plot(v, topn = 3))
  expect_error(plot(MixtureDistribution$new(list(Binomial$new(), Normal$new()))))
})
