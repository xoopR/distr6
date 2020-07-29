library(testthat)

#----------
# Setup
#----------
dexpo <- function(x) {
  m1 <- self$getParameterValue("rate")
  m2 <- exp(-1 * self$getParameterValue("rate") * x)
  return(m1 * m2)
}
cexpo <- function(x) {
  return(1 - exp(-self$getParameterValue("rate") * x))
}
ps <- ParameterSet$new(
  id = list("rate", "scale", "test"), value = list(1, 1, 0),
  support = list(PosReals$new(zero = T), PosReals$new(zero = T), Interval$new(0, 5)),
  settable = list(TRUE, FALSE, FALSE),
  description = list("Arrival rate", "Scale parameter", "testpar")
)
ps$addDeps("rate", "scale", function(self) list(scale = self$getParameterValue("rate")^-1))
ps$addDeps("scale", "rate", function(self) list(rate = self$getParameterValue("scale")^-1))

cont_pdf <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  pdf = dexpo,
  parameters = ps
)
cont_cdf <- Distribution$new("Continuous Test", "ContTest",
  support = PosReals$new(),
  symmetric = TRUE, type = PosReals$new(zero = T),
  cdf = cexpo,
  parameters = ps
)

dgeo <- function(x) {
  (1 - 0.5)^x * 0.5
}

pgeo <- function(x) {
  1 - (0.5^(x + 1)) # nolint
}

disc_pdf <- Distribution$new("Discrete Test",
  support = Interval$new(0, Inf,
    type = "[)",
    class = "integer"
  ),
  symmetric = FALSE, type = Naturals$new(),
  pdf = dgeo
)
disc_cdf <- Distribution$new("Discrete Test",
  support = Interval$new(0, Inf,
    type = "[)",
    class = "integer"
  ),
  symmetric = FALSE, type = Naturals$new(),
  cdf = pgeo
)

#----------
# basics
#----------
test_that("constructor", {
  expect_error(FunctionImputation$new()$decorate(MultivariateNormal$new()), "univariate")
  expect_equal(expect_message(
    FunctionImputation$new()$decorate(Binomial$new(decorators = "FunctionImputation")),
    "already decorated"), FunctionImputation$new())
})

test_that("method", {
  expect_equal(FunctionImputation$new()$methods, c(".rand", ".quantile", ".cdf", ".pdf"))
})

#----------
# pdf checks
#----------

test_that("basic pdf checks", {
  expect_equal(isPdf(cont_pdf), 1L)
  expect_equal(isCdf(cont_pdf), 0L)
  expect_equal(isQuantile(cont_pdf), 0L)
  expect_equal(isRand(cont_pdf), 0L)

  expect_silent(cont_pdf$pdf(1))
  expect_null(cont_pdf$cdf(1))
  expect_null(cont_pdf$quantile(1))
  expect_null(cont_pdf$rand(1))
  expect_message(decorate(cont_pdf, "FunctionImputation", n = 50000))
  expect_message(decorate(disc_pdf, "FunctionImputation", n = 50000))
  expect_silent(cont_pdf$pdf(1))
  expect_message(cont_pdf$cdf(1))
  expect_message(cont_pdf$quantile(0.42))
  expect_message(cont_pdf$rand(1))

  expect_equal(isPdf(cont_pdf), 1L)
  expect_equal(isCdf(cont_pdf), -1L)
  expect_equal(isQuantile(cont_pdf), -1L)
  expect_equal(isRand(cont_pdf), -1L)

  expect_equal(cont_pdf$.__enclos_env__$private$n_grid, 50000)
  expect_equal(disc_pdf$.__enclos_env__$private$n_grid, 50000)
})

#----------
# pdf2cdf
#----------

test_that("continuous pdf2cdf", {
  expect_equal(cont_pdf$pdf(1), dexp(1))
  expect_message(expect_equal(cont_pdf$cdf(1:3), pexp(1:3)))
})

test_that("discrete pdf2cdf", {
  expect_equal(disc_pdf$pdf(1), dgeom(1, prob = 0.5))
  expect_message(expect_equal(disc_pdf$cdf(0:10), pgeom(0:10, prob = 0.5)))
})

#----------
# pdf2quantile
#----------

test_that("continuous pdf2quantile", {
  expect_message(expect_rounded_equal(cont_pdf$quantile(c(0.2, 0.42, 0.6)),
    qexp(c(0.2, 0.42, 0.6)),
    dp = 4
  ))
})

test_that("discrete pdf2quantile", {
  expect_message(expect_equal(
    disc_pdf$quantile(c(0.2, 0.42, 0.6)),
    qgeom(c(0.2, 0.42, 0.6), prob = 0.5)
  ))
})

#----------
# pdf2rand
#----------

test_that("continuous pdf2quantile", {
  set.seed(2)
  r <- cont_pdf$rand(1000)
  t <- round(rexp(1000), 1)
  expect_warning(expect_true(ks.test(r, t)$p.value > 0.05))
  expect_equal(length(r), 1000)
  expect_true(all(r >= cont_pdf$inf))
  expect_true(all(r <= cont_pdf$sup))
})

test_that("discrete pdf2quantile", {
  set.seed(1)
  r <- disc_pdf$rand(10000)
  t <- rgeom(10000, 0.5)
  expect_warning(expect_true(ks.test(r, t)$p.value > 0.05))
  expect_equal(length(r), 10000)
  expect_true(all(r >= disc_pdf$inf))
  expect_true(all(r <= disc_pdf$sup))
})

#----------
# cdf checks
#----------

test_that("basic cdf checks", {
  expect_equal(isPdf(cont_cdf), 0L)
  expect_equal(isCdf(cont_cdf), 1L)
  expect_equal(isQuantile(cont_cdf), 0L)
  expect_equal(isRand(cont_cdf), 0L)

  expect_null(cont_cdf$pdf(1))
  expect_silent(cont_cdf$cdf(1))
  expect_null(cont_cdf$quantile(0.42))
  expect_null(cont_cdf$rand(1))
  expect_message(decorate(cont_cdf, "FunctionImputation", n = 10000))
  expect_message(decorate(disc_cdf, "FunctionImputation", n = 10000))
  expect_message(cont_cdf$pdf(1))
  expect_silent(cont_cdf$cdf(1))
  expect_message(cont_cdf$quantile(0.42))
  expect_message(cont_cdf$rand(1))

  expect_equal(isPdf(cont_cdf), -1L)
  expect_equal(isCdf(cont_cdf), 1L)
  expect_equal(isQuantile(cont_cdf), -1L)
  expect_equal(isRand(cont_cdf), -1L)

  expect_equal(cont_cdf$.__enclos_env__$private$n_grid, 10000)
  expect_equal(disc_cdf$.__enclos_env__$private$n_grid, 10000)
})
#----------
# cdf2pdf
#----------

test_that("continuous cdf2pdf", {
  expect_message(expect_equal(cont_cdf$pdf(1), dexp(1)))
  expect_message(expect_equal(cont_cdf$pdf(1:3), dexp(1:3)))
  expect_message(expect_equal(cont_cdf$pdf(1:3, log = TRUE), dexp(1:3, log = TRUE)))
})

test_that("discrete cdf2pdf", {
  expect_equal(disc_cdf$pdf(1), dgeom(1, prob = 0.5))
  expect_equal(disc_cdf$cdf(0:10), pgeom(0:10, prob = 0.5))
})

#----------
# cdf2quantile
#----------

test_that("continuous cdf2quantile", {
  expect_message(expect_rounded_equal(cont_cdf$quantile(c(0.2, 0.42, 0.6)),
    qexp(c(0.2, 0.42, 0.6)),
    dp = 3
  ))
})

test_that("discrete cdf2quantile", {
  expect_message(expect_equal(
    disc_cdf$quantile(c(0.2, 0.42, 0.6)),
    qgeom(c(0.2, 0.42, 0.6), prob = 0.5)
  ))
})

#----------
# cdf2rand
#----------

test_that("continuous cdf2rand", {
  set.seed(2)
  r <- cont_cdf$rand(100)
  t <- rexp(100)
  expect_true(ks.test(r, t)$p.value > 0.05)
  expect_equal(length(r), 100)
  expect_true(all(r >= cont_cdf$inf))
  expect_true(all(r <= cont_cdf$sup))
})

test_that("discrete cdf2rand", {
  set.seed(1)
  r <- disc_cdf$rand(100)
  t <- rgeom(100, 0.5)
  expect_warning(expect_true(ks.test(r, t)$p.value > 0.05))
  expect_equal(length(r), 100)
  expect_true(all(r >= disc_cdf$inf))
  expect_true(all(r <= disc_cdf$sup))
})

#----------
# cdf2rand
#----------

test_that("continuous quantile2rand", {
  set.seed(2)
  cont_cdf <- Distribution$new("Continuous Test", "ContTest",
                               support = PosReals$new(),
                               symmetric = TRUE, type = PosReals$new(zero = T),
                               cdf = cexpo, quantile = function(p) rep(0.421, length(p)),
                               parameters = ps
  )
  expect_null(cont_cdf$rand(10), 10)
  expect_message(decorate(cont_cdf, "FunctionImputation"))
  expect_equal(cont_cdf$rand(10), rep(0.421, 10))
})
