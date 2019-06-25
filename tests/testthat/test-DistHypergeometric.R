library(testthat)

context("Hypergeometric distribution")

test_that("parameterisation",{
    expect_silent(Hypergeometric$new())
    expect_silent(Hypergeometric$new(size = 10, success = 5, draws=2))
    expect_equal(Hypergeometric$new(size = 10, success = 5, draws=2)$getParameterValue("size"), 10)
    expect_equal(Hypergeometric$new(size = 10)$getParameterValue("size"), 10)
    expect_silent(Hypergeometric$new(size =10))
    expect_silent(Hypergeometric$new(success = 5))
    expect_silent(Hypergeometric$new(draws = 2))
})

test_that("properties & traits",{
    expect_equal(Hypergeometric$new()$valueSupport(), "discrete")
    expect_equal(Hypergeometric$new()$variateForm(), "univariate")
    expect_equal(Hypergeometric$new()$symmetry(), "symmetric")
    expect_equal(Hypergeometric$new()$sup(), 10)
    expect_equal(Hypergeometric$new()$inf(), 0)
    expect_equal(Hypergeometric$new()$dmax(), Inf)
    expect_equal(Hypergeometric$new()$dmin(), 2.220446e-16)
})


H = Hypergeometric$new(size=10,success = 5,draws=6)
test_that("statistics",{
    expect_equal(H$mean(), 3)
    expect_equal(round(H$var(),3), 0.667)
    expect_equal(H$skewness(), 0)
    expect_equal(round(H$kurtosis(T),3), -0.045)
    expect_equal(round(H$kurtosis(F),3), 2.955)
    expect_equal(H$pdf(1), dhyper(x=1,5,5,6))
    expect_equal(H$cdf(1), phyper(q=1,5,5,6))
    expect_equal(H$quantile(0.324), qhyper(0.324,5,5,6))
    expect_silent(H$rand(10))
})



