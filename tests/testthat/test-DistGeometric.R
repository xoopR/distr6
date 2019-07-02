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


g = Geometric$new()
test_that("statistics",{
    expect_equal(Geometric$new()$mean(), 1)
    expect_equal(Geometric$new(trials = T)$mean(), 2)
    expect_equal(g$variance(), 2)
    expect_equal(g$skewness(), 1.5/sqrt(0.5))
    expect_equal(g$kurtosis(T), 6.5)
    expect_equal(g$kurtosis(F), 9.5)
    expect_equal(g$mode(),0)
    expect_equal(Geometric$new(trials = T)$mode(),1)
    expect_equal(round(g$entropy(exp(1)), 5), round((-0.5 * log(0.5) - 0.5 * log(0.5)) * 2,5))
    expect_equal(g$mgf(1), 0.5/(1-0.5*exp(1)))
    expect_equal(Geometric$new(trials = T)$mgf(0.1), (0.5*exp(0.1))/(1-0.5*exp(0.1)))
    expect_equal(Geometric$new(trials = T)$mgf(1), NaN)
    expect_equal(g$cf(1), 0.5/(1-0.5*exp(1i)))
    expect_equal(Geometric$new(trials = T)$cf(0.1), (0.5*exp(0.1i))/(1-0.5*exp(0.1i)))
    expect_equal(g$pgf(3),-1)
    expect_equal(Geometric$new(trials = T)$pgf(3), -3)

    expect_equal(g$pdf(1), dgeom(1, 0.5))
    expect_equal(g$cdf(1), pgeom(1, 0.5))
    expect_equal(g$quantile(0.324), qgeom(0.324, 0.5))
    expect_silent(g$rand(10))

    expect_equal(Geometric$new(trials = T)$pdf(1), dgeom(2, 0.5))
    expect_equal(Geometric$new(trials = T)$cdf(1), pgeom(2, 0.5))
    expect_equal(Geometric$new(trials = T)$quantile(0.324), qgeom(0.324, 0.5)+1)
    expect_silent(Geometric$new(trials = T)$rand(10))
})
