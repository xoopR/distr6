library(testthat)

test_that("initialize", {
  p <- Binomial$new()$parameters()
  expect_silent(ParameterSetCollection$new(lst = list(Binom1 = p, Binom2 = p)))
  pc <- ParameterSetCollection$new(Binom1 = p, Binom2 = p)
  expect_equal(getR6Class(pc), "ParameterSetCollection")
  expect_equal(class(pc$.__enclos_env__$private$.parametersets), "list")
  expect_equal(length(pc$.__enclos_env__$private$.parametersets), 2)
  expect_equal(names(pc$.__enclos_env__$private$.parametersets), c("Binom1", "Binom2"))
})

test_that("getters", {
  pc <- ParameterSetCollection$new(
    Geom = getParameterSet.Geometric(),
    Binom = getParameterSet.Binomial()
  )
  expect_equal(pc$getParameterValue("Binom_prob"), 0.5)
  expect_equal(pc$getParameterValue("Geom_prob"), 0.5)
  expect_equal(
    pc$getParameterSupport("Binom_prob"),
    Binomial$new()$parameters()$getParameterSupport("prob")
  )

  expect_equal(pc$parameterSets, list(
    Geom = getParameterSet.Geometric(),
    Binom = getParameterSet.Binomial()
  ))

  expect_error(pc$getParameterValue("Norm"), "not in this")
  expect_error(pc$parameters("Norm"), "is not a")
  expect_equal(pc$parameters("Geom_prob"),
               data.table::data.table(id = "Geom_prob", value = list(0.5),
                                      support = list(Interval$new(0, 1, type = "(]")),
                                      settable = TRUE, description = "Probability of success"))
})

test_that("setters", {
  pc <- ParameterSetCollection$new(
    Geom = Geometric$new()$parameters(),
    Binom = Binomial$new()$parameters()
  )
  expect_equal(pc$getParameterValue("Binom_prob"), 0.5)
  expect_silent(pc$setParameterValue(Binom_prob = 1))
  expect_equal(pc$getParameterValue("Binom_prob"), 1)
  expect_equal(pc$getParameterValue("Binom_qprob"), 0)
})

test_that("clone_shallow", {
  pc <- ParameterSetCollection$new(
    Geom = Geometric$new()$parameters(),
    Binom = Binomial$new()$parameters()
  )
  pc2 <- pc$clone()
  expect_equal(pc$getParameterValue("Binom_prob"), pc2$getParameterValue("Binom_prob"))
  pc$setParameterValue(Binom_prob = 0.2)
  expect_equal(pc$getParameterValue("Binom_prob"), 0.2)
  expect_equal(pc2$getParameterValue("Binom_prob"), 0.2)
  pc2$setParameterValue(Binom_prob = 0.4)
  expect_equal(pc$getParameterValue("Binom_prob"), 0.4)
  expect_equal(pc2$getParameterValue("Binom_prob"), 0.4)
})

test_that("clone_deep", {
  pc <- ParameterSetCollection$new(
    Geom = Geometric$new()$parameters(),
    Binom = Binomial$new()$parameters()
  )
  pc2 <- pc$clone(deep = TRUE)
  expect_equal(pc$getParameterValue("Binom_prob"), pc2$getParameterValue("Binom_prob"))
  pc$setParameterValue(Binom_prob = 0.2)
  expect_equal(pc$getParameterValue("Binom_prob"), 0.2)
  expect_equal(pc2$getParameterValue("Binom_prob"), 0.5)
  pc2$setParameterValue(Binom_prob = 0.4)
  expect_equal(pc$getParameterValue("Binom_prob"), 0.2)
  expect_equal(pc2$getParameterValue("Binom_prob"), 0.4)
})

test_that("deps", {
  pc <- ParameterSetCollection$new(
    Geom = getParameterSet.Geometric(),
    Binom = getParameterSet.Binomial()
  )
  expect_equal(pc$deps, list(
    Geom = getParameterSet.Geometric()$deps,
    Binom = getParameterSet.Binomial()$deps
  ))
  expect_error(pc$addDeps())
})

test_that("print", {
  pc <- ParameterSetCollection$new(
    Geom = getParameterSet.Geometric(),
    Binom = getParameterSet.Binomial()
  )
  expect_output(pc$print())
})

test_that("merge", {
  b <- Binomial$new()
  g <- Geometric$new()
  psc <- ParameterSetCollection$new(Binom = b$parameters())
  psc2 <- ParameterSetCollection$new(Geom = g$parameters())
  expect_equal(psc$merge(psc2)$parameters(),
               ParameterSetCollection$new(Binom = b$parameters(), Geom = g$parameters()))
})
