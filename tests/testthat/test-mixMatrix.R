m1 <- as.Distribution(
    t(apply(matrix(runif(25), 5, 5, FALSE,
                    list(NULL, 1:5)), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

test_that("can't mixturise if different rows", {
  set.seed(1)

  m2 <- as.Distribution(
    t(apply(matrix(runif(20), 4, 5, FALSE,
                    list(NULL, 1:5)), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

  expect_error(mixMatrix(list(m1, m2)), "different number of rows")
})

test_that("can mixturise otherwise", {
  # unequal times
  m2 <- as.Distribution(
    t(apply(matrix(runif(25), 5, 5, FALSE,
                    list(NULL, sort(sample(1:10, 5)))), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

  m3 <- mixMatrix(list(m1, m2), c(0.8, 0.2))
  expect_equal(m3$cdf(1:10), m1$cdf(1:10) * 0.8 + m2$cdf(1:10) * 0.2)

  m2 <- as.Distribution(
    t(apply(matrix(runif(25), 5, 5, FALSE,
                    list(NULL, 1:5)), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

  # uniform
  m3 <- mixMatrix(list(m1, m2))
  expect_equal(m3$cdf(1:5), (m1$cdf(1:5) + m2$cdf(1:5)) / 2)

  # not uniform
  m3 <- mixMatrix(list(m1, m2), c(0.1, 0.9))
  expect_equal(m3$cdf(1:5), m1$cdf(1:5) * 0.1 + m2$cdf(1:5) * 0.9)
})

test_that("works with Arrdist", {
  m4 <- as.Distribution(
    t(apply(matrix(runif(4), 2, 2, FALSE,
      list(NULL, 1:2)), 1,
      function(x) x / sum(x))),
    fun = "pdf"
  )

  pdf5 <- runif(16)
  arr5 <- array(pdf5, c(2, 2, 4), list(NULL, 1:2, NULL))
  arr5 <- aperm(apply(arr5, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  darr5 <- Arrdist$new(pdf = arr5, which.curve = "mean")

  pdf6 <- runif(16)
  arr6 <- array(pdf6, c(2, 2, 4), list(NULL, 1:2, NULL))
  arr6 <- aperm(apply(arr6, c(1, 3), function(x) x / sum(x)), c(2, 1, 3))
  darr6 <- Arrdist$new(pdf = arr6, which.curve = 3)

  expect_silent(mixMatrix(list(m4, darr5, darr6)))
  expect_true(inherits(mixMatrix(list(darr5, darr6)), "Matdist"))
})
