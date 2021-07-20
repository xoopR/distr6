context("qqplot")

test_that("silent", {
  expect_silent(qqplot(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15)))
  expect_silent(qqplot(rnorm(n = 3000, mean = 15, sd = sqrt(30)), rchisq(n = 3000, df = 15)))
  expect_silent(qqplot(rnorm(n = 20, mean = 15, sd = sqrt(30)),
                       c(3, 6, 8, 2, 0, 6, 2, 7, 9, 4, 3, 7, 4, 5, 11, 1, 10, 6, 6, 3)))
  expect_silent(qqplot(Normal$new(mean = 15, sd = sqrt(30)), rchisq(n = 3000, df = 15)))
  expect_equal(class(qqplot(rnorm(5), rnorm(5), plot = FALSE, npoints = 4)), "list")
  expect_equal(length(qqplot(rnorm(5), rnorm(5), plot = FALSE, npoints = 4)), 2)
  expect_equal(length(qqplot(rnorm(5), rnorm(5), plot = FALSE, npoints = 4)[[1]]), 4)
  expect_silent(qqplot(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15),
                       idline = FALSE))
})

test_that("error", {
  expect_error(qqplot(Normal$new(mean = 15, sd = sqrt(30)), "A"), "both be distributions")
})
