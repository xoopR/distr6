library(testthat)

test_that("autotest", {
  autotest_kernel(Tricube,
    shortname = "Tric",
    variance = 35 / 243,
    squared2Norm = 175 / 247,
    support = Interval$new(-1, 1),
    pdf = c(0.8616, 0.8642, 0.8616)
  )
})
