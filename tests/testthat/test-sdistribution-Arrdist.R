
test_that("[.Matdist", {
  set.seed(1)
  pdf = runif(400)
  arr = array(pdf, c(20, 10, 2), list(NULL, sort(sample(1:20, 10)), NULL))
  arr = aperm(apply(arr, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  darr = as.Distribution(arr, fun = "pdf")

  expect_equal(darr$strprint(), "Arrdist(20x10x2)")
  expect_equal(2, darr$getParameterValue('which.curve'))
  expect_error(darr[logical(20)])

  expect_distribution(darr[!logical(1), 1], "WeightedDiscrete")
  expect_distribution(darr[!logical(20), 1], "Matdist")
  expect_distribution(darr[c(TRUE, logical(19)), 1:2], "Arrdist")

  # WeightDisc
  wd = darr[1, 2]
  expect_distribution(wd, "WeightedDiscrete")

  # Matdist
  md = darr[1:2, 2]
  expect_distribution(md, "Matdist")

  # Arrdist
  darr1 = darr[1:3, 1:2]
  darr2 = darr[1, 1:2]
  expect_distribution(darr1, "Arrdist")
  expect_distribution(darr2, "Arrdist")

  # check that underlying cdfs and pdfs stay the same after subsetting
  expect_equal(unname(md$cdf(0:20)[, 1]), unname(wd$cdf(0:20)))
  expect_equal(unname(md$pdf(0:20)[, 1]), unname(wd$pdf(0:20)))

  expect_equal(unname(darr1$cdf(0:20)[, 1]), unname(md$cdf(0:20)[, 1]))
  expect_equal(unname(darr1$pdf(0:20)[, 1]), unname(md$pdf(0:20)[, 1]))
  expect_equal(unname(darr1$cdf(0:20)[, 2]), unname(md$cdf(0:20)[, 2]))
  expect_equal(unname(darr1$pdf(0:20)[, 2]), unname(md$pdf(0:20)[, 2]))

  expect_equal(unname(darr1$cdf(0:20)[, 2]), unname(darr$cdf(0:20)[, 2]))
  expect_equal(unname(darr1$pdf(0:20)[, 2]), unname(darr$pdf(0:20)[, 2]))
  expect_equal(unname(darr1$cdf(0:20)[, 3]), unname(darr$cdf(0:20)[, 3]))
  expect_equal(unname(darr1$pdf(0:20)[, 3]), unname(darr$pdf(0:20)[, 3]))

  expect_equal(unname(darr1$cdf(0:20)[, 1]), unname(darr2$cdf(0:20)[, 1]))
  expect_equal(unname(darr1$pdf(0:20)[, 1]), unname(darr2$pdf(0:20)[, 1]))
  expect_equal(unname(darr1$cdf(0:20)[, 2]), unname(darr2$cdf(0:20)[, 2]))
  expect_equal(unname(darr1$pdf(0:20)[, 2]), unname(darr2$pdf(0:20)[, 2]))
})
