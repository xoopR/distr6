library(testthat)

context("Multinomial distribution")

test_that("constructor",{
  expect_error(Multinomial$new())
  expect_error(Multinomial$new(size = 5))
  expect_error(Multinomial$new(probs = c(0.2)))
  expect_silent(Multinomial$new(size = 3, probs = c(0.1,0.4)))
})

mn = Multinomial$new(size = 3, probs = c(0.1,0.9))
test_that("parameters", {
  expect_equal(mn$getParameterValue("K"), 2)
  expect_equal(mn$getParameterValue("size"), 3)
  expect_equal(mn$getParameterValue("probs"), c(0.1,0.9))
})

test_that("properties & traits",{
  expect_equal(mn$valueSupport(), "discrete")
  expect_equal(mn$variateForm(), "multivariate")
  expect_equal(mn$symmetry(), "asymmetric")
  expect_equal(mn$inf(), 0)
  expect_equal(mn$sup(), 3)
  expect_equal(mn$dmin(), 0)
  expect_equal(mn$dmax(), 3)
})

test_that("normalise", {
  expect_equal(Multinomial$new(size = 1, probs = c(1,2))$getParameterValue("probs"), c(1/3,2/3))
  expect_equal(Multinomial$new(size = 1, probs = c(0.1,0.9))$getParameterValue("probs"),c(0.1,0.9))
})


probs = c(0.1, 0.2, 0.7)
mn = Multinomial$new(size = 3, prob = probs)
test_that("statistics",{
  expect_equal(mn$mean(), 3 * probs)
  expect_equal(diag(mn$var()), 3 * probs * (1-probs))
  expect_equal(mn$var(), matrix(c(0.27,-0.06,-0.21,-0.06,0.48,-0.42,
                                 -0.21,-0.42,0.63),nrow = 3))
  expect_equal(mn$skewness(), NaN)
  expect_equal(mn$kurtosis(T), NaN)
  expect_equal(mn$kurtosis(F), NaN)
  expect_equal(round(mn$entropy(), 5), 2.35928)
  expect_equal(mn$mgf(1:3), sum(exp(1:3)*probs)^3)
  expect_equal(mn$pgf(1:3), sum((1:3)*probs)^3)
  expect_equal(mn$cf(1:3), sum(exp((1:3) * 1i)*probs)^3)
  expect_error(mn$mode())
  expect_equal(mn$pdf(1,5,7), 0)
  expect_error(mn$pdf(c(1,7)))
  expect_equal(mn$pdf(1,1,1), dmultinom(x = c(1,1,1), prob = probs))
  expect_equal(Multinomial$new(probs=c(1,4),size=5)$pdf(c(1,2,0),c(4,3,5)),
               c(dmultinom(x = c(1,4), prob = c(1,4)),
                 dmultinom(x = c(2,3), prob = c(1,4)),
                 dmultinom(x = c(0,5), prob = c(1,4))))
  expect_silent(mn$rand(10))
})
