library(testthat)

test_that("autotest",{
  autotest_kernel(TriangularKernel,
                  shortname = "Tri",
                  variance = 1/6,
                  squared2Norm = 2/3,
                  support = Interval$new(-1,1),
                  pdf = c(0.9, 1, 0.9),
                  cdf = c(0.405, 0.5, 0.595))
})
