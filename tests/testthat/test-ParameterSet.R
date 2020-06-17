library(testthat)

context("ParameterSet")

test_that("initialize", {
  expect_error(as.ParameterSet(list()))
  expect_silent(as.ParameterSet(list(id = "Test", value = 2, support = list(Set$new(1:5)), settable = F)))
  expect_silent(as.ParameterSet(data.table::data.table(
    id = "Test", value = 2, support = list(Set$new(1:5)),
    settable = F,
    stringsAsFactors = F
  )))
  expect_silent(ParameterSet$new(
    id = list("A", "B"), value = list(1, 1),
    support = list(PosReals$new(), Reals$new()), settable = list(T, T),
    description = list("A", NULL)
  ))
  ps <- ParameterSet$new(
    id = list("A", "B"), value = list(1, 1),
    support = list(PosReals$new(), Reals$new()), settable = list(T, T),
    description = list("A", NULL)
  )
  expect_output(ps$print())
})


test_that("getters", {
  expect_silent(Binomial$new()$getParameterValue("prob"))
  expect_silent(Binomial$new()$parameters("prob"))
  expect_error(Binomial$new()$parameters("prodsdsb"))
  expect_silent(Binomial$new()$parameters())
  expect_error(Binomial$new()$getParameterValue("prob2"))
  expect_silent(Binomial$new()$parameters())
  expect_equal(Binomial$new()$parameters()$getParameterSupport("prob"), Interval$new(0, 1))
  expect_warning(expect_null(Binomial$new()$parameters()$getParameterSupport()))
  expect_error(expect_null(Binomial$new()$parameters()$getParameterSupport("sdsa")))
})

test_that("setters", {
  expect_error(Binomial$new()$setParameterValue(lst = list(size = 5.1)))
  expect_error(Binomial$new()$setParameterValue(size = 5.1))
  expect_equal(Binomial$new()$parameters()$setParameterValue(size = 5)$getParameterValue("size"), 5)
  expect_error(Binomial$new()$setParameterValue(lst = list(prob = 2)))
  expect_silent(Binomial$new()$setParameterValue(lst = list(prob = 0.6)))
  expect_silent(Binomial$new()$setParameterValue(lst = list(prob = 0.6)))
  expect_warning(expect_null(Binomial$new()$parameters()$setParameterValue(lst = list(sdsa = 2))))
  expect_error(Exponential$new() %>% setParameterValue(rate = 0))
  expect_error(Exponential$new() %>% setParameterValue(rate = Inf))
})

test_that("merge", {
  expect_error(Binomial$new()$parameters()$merge(Binomial$new()$parameters()))
  expect_silent(Binomial$new()$parameters()$merge(Exponential$new(rate = 1)$parameters()))
})

test_that("no parameters", {
  expect_null(UniformKernel$new()$parameters())
  expect_null(UniformKernel$new()$setParameterValue(lst = list(d = 2)))
  expect_null(UniformKernel$new()$getParameterValue("d"))
})

test_that("out of support", {
  expect_error(ParameterSet$new(
    id = list("a"), value = list(0), support = list(Set$new(1)),
    settable = list(TRUE)
  ), "does not lie")
  expect_error(ParameterSet$new(
    id = list("a"), value = list(c(0, 0)), support = list(Set$new(1)^2),
    settable = list(TRUE)
  ), "does not lie")

  ps <- ParameterSet$new(
    id = list("a", "b"), value = c(0, 1), support = list(Set$new(0, 1), Set$new(0, 1)),
    settable = list(TRUE, FALSE)
  )
  ps$addDeps("a", "b", function(self) self$getParameterValue("a") + 1)
  expect_error(ps$setParameterValue(a = 2), "does not lie")
  # expect_error(ps$setParameterValue(a = 1), "does not lie")

  ps <- ParameterSet$new(
    id = list("a", "b"), value = list(c(0, 0), c(1, 1)),
    support = list(Set$new(0, 1)^2, Set$new(0, 1)^2),
    settable = list(TRUE, FALSE)
  )
  ps$addDeps("a", "b", function(self) self$getParameterValue("a") + 1)
  expect_error(ps$setParameterValue(a = c(2, 2)), "does not lie")
  # expect_error(ps$setParameterValue(a = c(1,1)), "does not lie")
})

test_that("clone_shallow", {
  ps = getParameterSet.Binomial()
  ps2 = ps$clone()
  expect_equal(ps$getParameterValue("prob"), ps2$getParameterValue("prob"))
  ps$setParameterValue(prob = 0.2)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.2)
  ps2$setParameterValue(prob = 0.4)
  expect_equal(ps$getParameterValue("prob"), 0.4)
  expect_equal(ps2$getParameterValue("prob"), 0.4)
})

test_that("clone_deep", {
  ps = getParameterSet.Binomial()
  ps2 = ps$clone(deep = TRUE)
  expect_equal(ps$getParameterValue("prob"), ps2$getParameterValue("prob"))
  ps$setParameterValue(prob = 0.2)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.5)
  ps2$setParameterValue(prob = 0.4)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.4)
})
