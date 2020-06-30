library(testthat)

context("helpers")

test_that("assertThat", {
  expect_silent(assertThat(Binomial$new(), Binomial$new()$short_name == "Binom", "Not True"))
  expect_error(assertThat(Binomial$new(), Binomial$new()$short_name == "Dinom", "Not True"))
})

test_that("checkThat", {
  expect_true(checkThat(Binomial$new()$short_name == "Binom", "Not True"))
  expect_equal(checkThat(Binomial$new()$short_name == "Dinom", "Not True"), "Not True")
})

test_that("testThat", {
  expect_true(testThat(Binomial$new()$short_name == "Binom"))
  expect_false(testThat(Binomial$new()$short_name == "Dinom"))
})

test_that("isThat", {
  expect_true(isThat(Binomial$new()$short_name == "Binom"))
  expect_false(isThat(Binomial$new()$short_name == "Dinom"))
})

test_that("makeChecks", {
  expect_silent(makeChecks("Test", 1 == 1, "Error"))
})

test_that("getR6Class", {
  expect_equal(getR6Class(Binomial$new()), "Binomial")
  expect_equal(getR6Class(Binomial$new(), classname = F), Binomial)
})

test_that("stopwarn", {
  expect_warning(expect_null(stopwarn(error = "warn", "Warning")))
  expect_error(stopwarn(error = "stop", "Warning"))
})

test_that("testmessage", {
  expect_true(testMessage(message("Hi")))
  expect_warning(expect_false(testMessage(warning("Hi"))))
})

test_that("ifnerror", {
  expect_equal(ifnerror(stop("Error"), "Success", "Failure", silent = T), "Failure")
  expect_equal(ifnerror("Nerror", "Success", "Failure", silent = T), "Success")
  expect_warning(ifnerror(stop("Error"), "Success", "warn", silent = T))
  expect_error(ifnerror(stop("Error"), "Success", "stop", silent = T))
})

test_that("modal", {
  expect_equal(modal(c(1, 2, 2, 4, 5, 6, 7, 2, 4, 4, 2, 4, 2)), 2)
  expect_equal(modal(c(1, 2, 2, 4, 5, 6, 7, 2, 4, 4, 2, 4, 2, 4)), c(2, 4))
})

test_that("toproper", {
  expect_equal(toproper("a long SenTENCe"), "A Long Sentence")
  expect_equal(toproper("DifFERent-spLIT", split = "-"), "Different-Split")
})

test_that("assert_pkgload", {
  expect_error(assert_pkgload("dsad"), "The following")
  expect_silent(assert_pkgload("stats"))
})

test_that("pdq_helpers", {
  expect_error(pdq_point_assert(data = NULL), "Points to")
  expect_warning(pdq_point_assert(1, 2, self = Binomial$new(), data = NULL),
                 "Distribution is univariate")
  expect_warning(pdq_point_assert(self = Binomial$new(), data = data.frame(1, 2)),
                 "Distribution is univariate")
  expect_error(pdq_point_assert(1, self = Multinomial$new(), data = NULL),
                 "Distribution is multivariate")
  expect_error(pdq_point_assert(self = Multinomial$new(), data = data.frame(1)),
                 "Distribution is multivariate")
  checkmate::expect_data_table(pdqr_returner(matrix(1, nrow = 2, ncol = 2), FALSE, "A"),
                               nrows = 2, ncols = 2)
  expect_equal(colnames(pdqr_returner(matrix(1, nrow = 2, ncol = 2), FALSE, "A")),
               c("A.V1", "A.V2"))
  expect_error(call_C_base_pdqr("l", 1, vec = FALSE), "Function must")
  expect_error(call_C_base_pdqr("l", 1, vec = TRUE), "Function must")
})

test_that("oneword", {
  expect_silent(assertOneWord(c("a", "sfas")))
  expect_error(assertOneWord(c("a", "sf as")))
})

test_that("v_genfun", {
  expect_equal(v_genfun(1, function(x) x + 1), 2)
  expect_equal(v_genfun(c(1, 2), function(x) x + 1), 2:3)
})

test_that("abstract", {
  expect_error(abstract(1, "numeric"), "abstract class")
  expect_error(abstract(1, "numeric", "purple"), "purple")
})

test_that("rsapply", {
  expect_equal(rsapply(list(Binomial$new(), Normal$new()), pdf, 1),
               c(Binomial$new()$pdf(1), Normal$new()$pdf(1)))
  expect_equal(rsapply(list(Binomial$new(), Normal$new()), short_name, active = TRUE),
               c("Binom", "Norm"))
})
