library(testthat)

test_that("autotest",{
  autotest_kernel(Sigmoid,
                  shortname = "Sigm",
                  variance = (pi^2)/4,
                  squared2Norm = 2/(pi^2),
                  support = Reals$new(),
                  pdf = c(0.3167, 0.3183,  0.3167))
})
