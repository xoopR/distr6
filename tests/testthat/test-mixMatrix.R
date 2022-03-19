m1 <- as.Distribution(
    t(apply(matrix(runif(25), 5, 5, FALSE,
                    list(NULL, 1:5)), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

test_that("can't mixturise if different rows or columns", {
  set.seed(1)

  m2 <- as.Distribution(
    t(apply(matrix(runif(25), 5, 5, FALSE,
                    list(NULL, sort(sample(1:10, 5)))), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

  expect_error(mixMatrix(list(m1, m2), "uniform"), "mixturiseVector")

  m2 <- as.Distribution(
    t(apply(matrix(runif(20), 4, 5, FALSE,
                    list(NULL, 1:5)), 1,
            function(x) x / sum(x))),
    fun = "pdf"
  )

  expect_error(mixMatrix(list(m1, m2)), "different number of rows")
})

test_that("can't mixturise otherwise", {
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
