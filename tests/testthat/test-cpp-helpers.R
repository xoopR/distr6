library(testthat)

test_that("catch error", {
  expect_error(C_dpq("rnorm", 1, list(sd = 1, mean = 2)), "Function must start")
})

test_that("choose", {
  expect_equal(C_Choose(0, -2), 0)
  set.seed(1)
  mapply(function(x, y) expect_equal(C_Choose(x, y), choose(x, y)),
         x = round(runif(10, 0, 5)), y = round(runif(10, 0, 5)))
})
