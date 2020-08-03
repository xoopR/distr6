library(testthat)

test_that("initialize", {
  expect_error(as.ParameterSet(list()))
  expect_silent(as.ParameterSet(list(id = "Test", value = 2,
                                     support = list(Set$new(1:5)), settable = F)))
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

  expect_error(ParameterSet$new(id = "a", value = 1, support = Reals$new()^2), "does not lie")
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
  expect_null(expect_warning(Binomial$new()$getParameterValue(), "missing"))
})

test_that("setters", {
  expect_error(Binomial$new()$setParameterValue(lst = list(size = 5.1)))
  expect_error(Binomial$new()$setParameterValue(size = 5.1))
  expect_equal(Binomial$new()$parameters()$setParameterValue(size = 5)$getParameterValue("size"), 5)
  expect_error(Binomial$new()$setParameterValue(lst = list(prob = 2)))
  expect_silent(Binomial$new()$setParameterValue(lst = list(prob = 0.6)))
  expect_silent(Binomial$new()$setParameterValue(lst = list(prob = 0.6)))
  expect_error(Binomial$new()$parameters()$setParameterValue(lst = list(sdsa = 2)))
  expect_error(Exponential$new() %>% setParameterValue(rate = 0))
  expect_error(Exponential$new() %>% setParameterValue(rate = Inf))
  expect_error(ParameterSet$new(id = "a",
                                value = list(c(1, 2)),
                                support = Reals$new()^2)$setParameterValue(a = 1), "does not lie")
})

test_that("merge", {
  expect_error(Binomial$new()$parameters()$merge(Binomial$new()$parameters()))
  expect_silent(Binomial$new()$parameters()$merge(Exponential$new(rate = 1)$parameters()))
})

test_that("no parameters", {
  expect_equal(UniformKernel$new()$parameters(), ParameterSet$new())
  expect_equal(UniformKernel$new()$setParameterValue(lst = list(d = 2)), UniformKernel$new())
  expect_error(UniformKernel$new()$getParameterValue("d"))
})

test_that("addDeps", {
  ps <- ParameterSet$new(
    id = list("a", "b"), value = c(0, 1), support = list(Set$new(0, 1), Set$new(0, 1)),
    settable = list(TRUE, FALSE)
  )
  expect_error(ps$addDeps("a", "c", function(self) x), "subset of")
})

test_that("addTrafos", {
  ps <- ParameterSet$new(
    id = list("a", "b"), value = c(0, 1), support = list(Set$new(0, 1), Set$new(0, 1)),
    settable = list(TRUE, FALSE)
  )
  expect_error(ps$addTrafos("c", function(x) x), "'c' is not")
  expect_silent(ps$addTrafos("a", function(x, self) x + 1))
  expect_warning(ps$addTrafos("a", function(x, self) x + 2), "overwritten")
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
  ps <- getParameterSet.Binomial()
  ps2 <- ps$clone()
  expect_equal(ps$getParameterValue("prob"), ps2$getParameterValue("prob"))
  ps$setParameterValue(prob = 0.2)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.2)
  ps2$setParameterValue(prob = 0.4)
  expect_equal(ps$getParameterValue("prob"), 0.4)
  expect_equal(ps2$getParameterValue("prob"), 0.4)
})

test_that("clone_deep", {
  ps <- getParameterSet.Binomial()
  ps2 <- ps$clone(deep = TRUE)
  expect_equal(ps$getParameterValue("prob"), ps2$getParameterValue("prob"))
  ps$setParameterValue(prob = 0.2)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.5)
  ps2$setParameterValue(prob = 0.4)
  expect_equal(ps$getParameterValue("prob"), 0.2)
  expect_equal(ps2$getParameterValue("prob"), 0.4)
})

test_that("deprecated", {
  expect_warning(ParameterSet$new(updateFunc = function(x) x, id = "a", value = 2,
                                  support = Reals$new()), "deprecated")
})

test_that("c", {
  ps1 <- getParameterSet.Degenerate()
  ps2 <- getParameterSet.Poisson()
  expect_equal(as.data.table(c(ps1, ps2)),
               data.table(id = c("mean", "rate"), value = list(0, 1),
                          support = list(reals, pos_reals),
                          settable = c(TRUE, TRUE),
                          description = c("Location Parameter", "Arrival Rate")))
  ps3 <- getParameterSet.Binomial()
  expect_error(c(ps3, ps3))
  expect_error(c(ps3, ps3, prefix.names = "Binom"), "length")
  expect_silent(c(ps3, ps3, prefix.names = c("Binom1", "Binom2")))
})

test_that("extract", {
  ps <- getParameterSet.Binomial()
  expect_equal(ps["prob"], ParameterSet$new(id = "prob", value = 0.5, support = Interval$new(0, 1),
                                            description = "Probability of Success"))
  ps2 <- expect_silent(c(ps, ps, prefix.names = c("Binom1", "Binom2")))
  expect_equal(ps2["Binom1_prob"], ParameterSet$new(id = "Binom1_prob", value = 0.5,
                                                    support = Interval$new(0, 1),
                                            description = "Probability of Success"))
  expect_equal(ps2["Binom1_prob", prefix = "Binom1"], ParameterSet$new(id = "prob", value = 0.5,
                                                    support = Interval$new(0, 1),
                                                    description = "Probability of Success"))
  exp <- getParameterSet.Binomial()
  exp$.__enclos_env__$private$.deps <- data.table(x = character(), y = character(), fun = list())
  expect_equal(ps2["Binom1_"], exp)
})
