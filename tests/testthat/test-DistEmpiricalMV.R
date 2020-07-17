library(testthat)

test_that("autotest", {
  autotest_sdistribution(
    sdist = EmpiricalMV,
    pars = list(matrix(1:20, ncol = 2)),
    traits = list(
      valueSupport = "discrete",
      variateForm = "multivariate",
      type = Reals$new()^"n"
    ),
    support = Tuple$new(1:10) * Tuple$new(11:20),
    symmetry = "asymmetric",
    mean = c(mean(1:10), mean(11:20)),
    median = NaN,
    variance = c(var(1:10), var(11:20)) * 9 / 10,
    vectorise = FALSE
  )
})

test_that("manual", {
  dist <- EmpiricalMV$new(data.frame(1:10, 1:10))
  expect_null(expect_warning(dist$setParameterValue(sd = 2), "Data cannot"))
  expect_error(EmpiricalMV$new(data.frame(1:10)), "use Empirical")
})

test_that("multivariate pdf", {
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$pdf(c(1, 2), c(11, 12)),
    c(1 / 10, 1 / 10)
  )
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$pdf(c(1, 2), c(11, 12), log = TRUE),
    log(c(1 / 10, 1 / 10))
  )
})

test_that("multivariate cdf", {
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$cdf(c(3, 7), c(12, 14)),
    c(2 / 10, 4 / 10)
  )
  expect_equal(
    EmpiricalMV$new(matrix(1:20, ncol = 2))$cdf(c(3, 7), c(12, 14),
                                                lower.tail = FALSE, log.p = TRUE),
    log(1 - c(2 / 10, 4 / 10))
  )
})
