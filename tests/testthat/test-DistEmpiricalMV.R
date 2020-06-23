library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = EmpiricalMV,
    pars = list(matrix(1:20, ncol = 2)),
    traits = list(
      valueSupport = "discrete",
      variateForm = "multivariate",
      type = Reals$new()^2
    ),
    support = do.call(setproduct, as.Tuple(matrix(1:20, ncol = 2))),
    symmetry = "asymmetric",
    mean = c(mean(1:10), mean(11:20)),
    median = NaN,
    variance = c(var(1:10), var(11:20)) * 9 / 10,
    vectorise = FALSE
  )
})

test_that("multivariate pdf", {
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$pdf(c(1, 2), c(11, 12)),
    c(1 / 10, 1 / 10)
  )
})

test_that("multivariate cdf", {
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$cdf(c(3, 7), c(12, 14)),
    c(2 / 10, 4 / 10)
  )
})
