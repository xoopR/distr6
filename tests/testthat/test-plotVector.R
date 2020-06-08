library(testthat)

test_that("valueSupport/variateForm", {
  expect_error(plot(MixtureDistribution$new(list(Binomial$new(), Normal$new()))))
})
