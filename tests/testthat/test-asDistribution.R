test_that("as.Distribution errors when expected", {
  mat <- matrix(rep(0.1, 200), 20, 10)
  expect_error(as.Distribution(mat, vector = TRUE), "'obj' must have")
  colnames(mat) <- 1:10
  expect_error(as.Distribution(mat, "surv", vector = TRUE), "'fun' should be one of")
})

test_that("as.Distribution works with pdf expected", {
  mat <- matrix(rep(0.1, 200), 20, 10)
  colnames(mat) <- 1:10

  wd <- as.Distribution(mat, fun = "pdf", vector = TRUE)

  expect_R6_class(wd, "VectorDistribution")
  expect_equal(as.character(unique(wd$modelTable$Distribution)), "WeightedDiscrete")

  expect_equal(
    vapply(wd$getParameterValue("x"), identity, numeric(10)),
    matrix(1:10, 10, 20, FALSE, list(NULL, paste0("WeightDisc", 1:20)))
  )

  expect_equal(
    vapply(wd$getParameterValue("pdf"), identity, numeric(10)),
    matrix(t(mat), 10, 20, FALSE, list(1:10, paste0("WeightDisc", 1:20)))
  )
})

test_that("as.Distribution works with cdf expected", {
  mat <- matrix(rep(0.1, 200), 20, 10)
  colnames(mat) <- 1:10

  mat <- t(apply(mat, 1, function(x) cumsum(x)))

  wd <- as.Distribution(mat, fun = "cdf", vector = TRUE)

  expect_R6_class(wd, "VectorDistribution")
  expect_equal(as.character(unique(wd$modelTable$Distribution)), "WeightedDiscrete")

  expect_equal(
    vapply(wd$getParameterValue("x"), identity, numeric(10)),
    matrix(1:10, 10, 20, FALSE, list(NULL, paste0("WeightDisc", 1:20)))
  )

  expect_equal(
    vapply(wd$getParameterValue("cdf"), identity, numeric(10)),
    matrix(t(mat), 10, 20, FALSE, list(1:10, paste0("WeightDisc", 1:20)))
  )
})


test_that("as.Distribution works with Matdist", {
  mat <- matrix(rep(0.1, 200), 20, 10)
  colnames(mat) <- 1:10
  mat <- t(apply(mat, 1, function(x) cumsum(x)))
  expect_distribution(as.Distribution(mat, fun = "cdf"), "Matdist")

  mat <- matrix(rep(0.1, 200), 20, 10)
  colnames(mat) <- 1:10
  expect_distribution(as.Distribution(mat, fun = "pdf"), "Matdist")
})
