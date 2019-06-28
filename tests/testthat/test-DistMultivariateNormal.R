library(testthat)

context("Multinomial distribution")

test_that("constructor",{
  expect_silent(MultivariateNormal$new())
  expect_silent(MultivariateNormal$new(mean = c(0,0,0), prec = c(1,0,0,0,1,0,0,0,1)))
  expect_error(MultivariateNormal$new(mean = 5))
  expect_warning(MultivariateNormal$new(mean = c(2,3), cov = matrix(c(1,3,4))))
})

mvn = MultivariateNormal$new(mean = c(1,7,3),cov = c(1,0,0,0,1,0,0,0,1))
test_that("parameters", {
  expect_equal(mvn$getParameterValue("K"), 3)
  expect_equal(mvn$getParameterValue("mean"), c(1,7,3))
  expect_equal(mvn$getParameterValue("cov"), matrix(c(1,0,0,0,1,0,0,0,1),nrow=3))
  expect_equal(mvn$getParameterValue("prec"), matrix(c(1,0,0,0,1,0,0,0,1),nrow=3))
})

test_that("properties & traits",{
  expect_equal(mvn$valueSupport(), "continuous")
  expect_equal(mvn$variateForm(), "multivariate")
  expect_equal(mvn$symmetry(), "asymmetric")
  expect_equal(mvn$inf(), -Inf)
  expect_equal(mvn$sup(), Inf)
  expect_equal(mvn$dmin(), -Inf)
  expect_equal(mvn$dmax(), Inf)
})

test_that("statistics",{
  expect_equal(mvn$mean(), c(1,7,3))
  expect_equal(mvn$mode(), c(1,7,3))
  expect_equal(diag(mvn$var()), c(1,1,1))
  expect_equal(mvn$cor(), mvn$var())

  expect_error(mvn$skewness())
  expect_error(mvn$kurtosis())
  expect_error(mvn$pgf(1:3))

  expect_equal(mvn$entropy(), 0.5*log(det(2*pi*(exp(1)*mvn$var())),2))
  expect_equal(mvn$mgf(1:3), exp(matrix(c(1,7,3),nrow=1)%*%matrix(1:3,ncol=1) + (0.5 * matrix(1:3,nrow=1)%*%mvn$var()%*%matrix(1:3,ncol=1))))
  expect_error(mvn$mgf(1))
  expect_equal(mvn$cf(1:3), exp(1i * matrix(c(1,7,3),nrow=1)%*%matrix(1:3,ncol=1) + (0.5 * matrix(1:3,nrow=1)%*%mvn$var()%*%matrix(1:3,ncol=1))))
  expect_error(mvn$cf(1))


  expect_equal(MultivariateNormal$new(mean = 0, cov = 1)$pdf(1:5),dnorm(1:5))
  expect_equal(signif(mvn$pdf(1,2,3),3), 2.366e-07)
  expect_equal(signif(mvn$pdf(1:2,2:3,3:4),3), c(2.366e-07, 7.835e-06))
  expect_equal(dim(mvn$rand(10)),c(10,3))
})
