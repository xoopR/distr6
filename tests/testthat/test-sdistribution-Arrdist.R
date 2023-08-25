skip_if_distr_not_installed(Arrdist)

test_that("autottest", {
  pdf = runif(16)
  arr = array(pdf, c(2, 2, 4), list(NULL, 1:2, NULL))
  arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  wd1 = dstr("WeightDisc", pdf = arr[1, , 3], x = as.numeric(colnames(arr)))
  wd2 = dstr("WeightDisc", pdf = arr[2, , 3], x = as.numeric(colnames(arr)))

  autotest_sdistribution(Arrdist,
    pars = list(pdf = arr, which.curve = 3),
    traits = list(
      valueSupport = "discrete", variateForm = "univariate",
      type = Reals$new()^"n"
    ),
    support = Set$new(1:2, class = "numeric"),
    symmetry = "asymmetric",
    mean = c(wd1$mean(), wd2$mean()),
    mode = c(wd1$mode(), wd2$mode()),
    median = c(wd1$median(), wd2$median()),
    variance = c(wd1$variance(), wd2$variance()),
    skewness = c(wd1$skewness(), wd2$skewness()),
    exkur = c(wd1$kurtosis(), wd2$kurtosis()),
    entropy = c(wd1$entropy(), wd2$entropy()),
    mgf = c(wd1$mgf(1), wd2$mgf(1)),
    cf = c(wd1$cf(1), wd2$cf(1)),
    pgf = c(wd1$pgf(1), wd2$pgf(1)),
    pdf = t(matrix(c(wd1$pdf(1:3), wd2$pdf(1:3)), 2, 3, TRUE, list(NULL, 1:3))),
    cdf = t(matrix(c(wd1$cdf(1:3), wd2$cdf(1:3)), 2, 3, TRUE, list(NULL, 1:3))),
    quantile = t(matrix(c(wd1$quantile(c(0.24, 0.42, 0.5)), wd2$quantile(c(0.24, 0.42, 0.5))),
                      2, 3, TRUE, dimnames = NULL)),
    vectorise = FALSE
  )
})

test_that("calculate mean", {
    pdf = runif(16)
    arr = array(pdf, c(2, 2, 4), list(NULL, 1:2, NULL))
    arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
    darr = as.Distribution(arr, fun = "pdf")

    expect_error(sprm(darr, list(which.curve = "measdfdn")), "does not lie")

    sprm(darr, list(which.curve = "mean"))
    expect_equal(darr$pdf(1:2), t(apply(gprm(darr, "pdf"), c(1, 2), mean)))
    expect_equal(darr$cdf(1:2), t(apply(gprm(darr, "cdf"), c(1, 2), mean)))
})

test_that("c.Arrdist", {
  set.seed(1)
  arr_list = replicate(3, {
    pdf = runif(400)
    arr = array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
    arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
    as.Distribution(arr, fun = "pdf")
  })
  arr1 = arr_list[[1]]
  arr2 = arr_list[[2]]
  arr3 = arr_list[[3]]
  carr = do.call(c, arr_list)

  expect_equal(carr$pdf(0:30), cbind(arr1$pdf(0:30), arr2$pdf(0:30), arr3$pdf(0:30)))
  expect_equal(carr$cdf(0:30), cbind(arr1$cdf(0:30), arr2$cdf(0:30), arr3$cdf(0:30)))
  expect_equal(carr$quantile(0.42),
               cbind(arr1$quantile(0.42), arr2$quantile(0.42), arr3$quantile(0.42)))
  r = carr$rand(50)
  expect_equal(dim(r), c(50, 60))
  expect_true(all(r <= 20))
  expect_true(all(r >= 1))
})

test_that("Arrdist basics", {
  # create Array
  pdf = runif(24)
  arr = array(pdf, c(2, 3, 4), list(NULL, 1:3, NULL))
  arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  darr = as.Distribution(arr, fun = "pdf")

  expect_distribution(darr, "Arrdist")
  expect_equal(darr$strprint(), "Arrdist(2x3x4)")
  expect_equal(0.5, gprm(darr, "which.curve"))
  sprm(darr, list(which.curve = 1))
  expect_equal(1, gprm(darr, "which.curve"))
  sprm(darr, list(which.curve = 0.9))
  expect_equal(0.9, gprm(darr, "which.curve"))
  sprm(darr, list(which.curve = "mean"))
  expect_equal("mean", gprm(darr, "which.curve"))
  expect_error(sprm(darr, list(which.curve = 6)), "third dimension")
})

test_that("[.Arrdist", {
  # create Array
  set.seed(1)
  pdf = runif(400)
  arr = array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
  arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  darr = as.Distribution(arr, fun = "pdf")

  # logical extraction
  expect_equal_distr(darr[logical(20)], darr)
  expect_distribution(darr[!logical(1), 1], "WeightedDiscrete")
  expect_distribution(darr[!logical(20), 1], "Matdist")
  expect_distribution(darr[c(TRUE, logical(19)), 1:2], "Arrdist")

  # extract by mean
  expect_equal(
    as.numeric(darr[, "mean"]$pdf(1)),
    apply(gprm(darr, "pdf")[, 1, ], 1, mean)
  )

  # compare extracted results

  expect_distribution(darr[1, 2], "WeightedDiscrete")
  expect_distribution(darr[1:2, 2], "Matdist")

  # row 1 curve 2
  wd1_cdf = unname(darr[1, 1]$cdf(0:20))
  wd1_pdf = unname(darr[1, 1]$pdf(0:20))
  # row 2 curve 2
  wd2_cdf = unname(darr[2, 2]$cdf(0:20))
  wd2_pdf = unname(darr[2, 2]$pdf(0:20))

  # check Matdist extraction matches WD
  expect_equal(unname(darr[1:2, 1]$cdf(0:20)[, 1]), wd1_cdf)
  expect_equal(unname(darr[1:2, 2]$cdf(0:20)[, 2]), wd2_cdf)

    # Arrdist
  darr1 = darr[1:2, 1:2]
  expect_distribution(darr1, "Arrdist")

  sprm(darr1, list(which.curve = 1))
  expect_equal(unname(darr1$cdf(0:20)[, 1]), wd1_cdf)
  expect_equal(unname(darr1$pdf(0:20)[, 1]), wd1_pdf)

  sprm(darr1, list(which.curve = 2))
  expect_equal(unname(darr1$cdf(0:20)[, 2]), wd2_cdf)
  expect_equal(unname(darr1$pdf(0:20)[, 2]), wd2_pdf)

  # edge cases
  small_darr = darr[1, 1:2]
  expect_equal(dim(small_darr$cdf(0:20)), c(21, 1))
  expect_equal(dim(small_darr$pdf(0:20)), c(21, 1))
  expect_equal(dim(small_darr$quantile(c(0.4, 0.5))), c(2, 1))
  expect_equal(dim(small_darr$rand(0:20)), c(21, 1))
})
