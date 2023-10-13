test_that("vector transformers", {
  pdf <- c(0.1, 0.2, 0.7)
  cdf <- cumsum(pdf)

  expect_equal(pdfcdf(pdf), cdf)
  expect_equal(cdfpdf(cdf), pdf)
})

test_that("matrix transformers", {
  pdf <- matrix(c(0.1, 0.2, 0.7, 0.2, 0.2, 0.6), 2, 3, TRUE)
  cdf <- t(apply(pdf, 1, cumsum))

  expect_equal(pdfcdf(pdf), cdf)
  expect_equal(cdfpdf(cdf), pdf)
})

test_that("array transformers", {
  pdf <- matrix(c(0.1, 0.2, 0.7, 0.2, 0.2, 0.6), 2, 3, TRUE)
  cdf <- t(apply(pdf, 1, cumsum))
  pdf <- array(pdf, c(2, 3, 5))
  cdf <- array(cdf, c(2, 3, 5))

  expect_equal(pdfcdf(pdf), cdf)
  expect_equal(cdfpdf(cdf), pdf)
})

test_that("transformers error when expected", {
  expect_error(pdfcdf(array(dim = c(1, 1, 1, 1))), "Expected maximum")
  expect_error(cdfpdf(array(dim = c(1, 1, 1, 1))), "Expected maximum")
})
