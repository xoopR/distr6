# install.packages("testthat")
library(testthat)

context("Geometric distribution")

test_that("properties & traits",{
    expect_equal(Geometric$new()$valueSupport(), "discrete")
    expect_equal(Geometric$new()$variateForm(), "univariate")
    expect_equal(Geometric$new()$symmetry(), "asymmetric")
    expect_equal(Geometric$new()$sup(), Inf)
    expect_equal(Geometric$new()$inf(), 0)
    expect_equal(Geometric$new()$dmax(), Inf)
    expect_equal(Geometric$new()$dmin(), 0)
})


g = Geometric$new(prob = 0.2)
test_that("statistics",{
    expect_equal(g$mean(), 5)
    expect_equal(g$var(), (1-0.2)/(0.2^2))
    expect_equal(g$skewness(), 0.9*sqrt(5))
    expect_equal(g$kurtosis(T), 6.05)
    expect_equal(g$kurtosis(F), 9.05)
    expect_equal(round(g$entropy(), 5), 3.60964)
    expect_equal(g$mgf(1), (0.2*exp(1))/(1-(1-0.2)*exp(1)))
    expect_equal(g$cf(1), (0.2*exp(1i))/(1-(1-0.2)*exp(1i)))
    expect_equal(g$pgf(1), 1)
    expect_equal(g$mode(),0)
    expect_equal(g$pdf(1), dgeom(1, 0.2))
    expect_equal(g$cdf(1), pgeom(1, 0.2))
    expect_equal(g$quantile(0.324), qgeom(0.324, 0.2))
    expect_silent(g$rand(10))
})