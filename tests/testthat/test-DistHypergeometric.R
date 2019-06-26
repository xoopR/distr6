library(testthat)

context("Hypergeometric distribution")

test_that("parameterisation",{
    expect_silent(Hypergeometric$new())
    expect_silent(Hypergeometric$new(size = 10, successes = 5, draws=2))
    expect_equal(Hypergeometric$new(size = 10, successes = 5, draws=2)$getParameterValue("size"), 10)
    expect_equal(Hypergeometric$new(size = 10)$getParameterValue("size"), 10)
    expect_silent(Hypergeometric$new(size =10))
    expect_silent(Hypergeometric$new(successes = 5))
    expect_silent(Hypergeometric$new(failures = 5))
    expect_equal(Hypergeometric$new(size = 10, failures = 2, draws=2)$getParameterValue("successes"), 8)
    expect_silent(Hypergeometric$new(draws = 2))
})

test_that("properties & traits",{
    expect_equal(Hypergeometric$new()$valueSupport(), "discrete")
    expect_equal(Hypergeometric$new()$variateForm(), "univariate")
    expect_equal(Hypergeometric$new()$symmetry(), "asymmetric")
    expect_equal(Hypergeometric$new()$sup(), 5)
    expect_equal(Hypergeometric$new()$inf(), 0)
    expect_equal(Hypergeometric$new()$dmax(), 5)
    expect_equal(Hypergeometric$new()$dmin(), 0)
})


H = Hypergeometric$new()
test_that("statistics",{
    expect_equal(H$mean(), 1)
    expect_equal(round(H$var(),7), 0.7346939)
    expect_equal(round(H$skewness(),7), 0.5833333)
    expect_equal(signif(H$kurtosis(T),7), -0.0750591)
    expect_equal(signif(H$kurtosis(F),7), 2.924941)
    expect_equal(H$pdf(1), dhyper(1,5,45,10))
    expect_equal(H$cdf(1), phyper(1,5,45,10))
    expect_equal(H$quantile(0.324), qhyper(0.324,5,45,10))
    expect_equal(length(H$rand(10)),10)
})



