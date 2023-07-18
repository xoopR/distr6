test_that("Function only accepts valid structures", {
  expect_error(dparse("t"), "Call 't' does not have a valid format. See documentation.")
  expect_error(dparse("n("), "Call 'n\\(' does not have a valid format. See documentation.")
  expect_error(dparse("GaMmA)"), "Call 'GaMmA)' does not have a valid format. See documentation.")
  expect_error(dparse("c2)("), "Call 'c2\\)\\(' does not have a valid format. See documentation.")
})

test_that("Parameter parsing works correctly", {
  #TODO: Add more per Github review
  expect_equal(dparse("n()")$strprint(), "Norm(mean = 0, var = 1)")
  expect_equal(dparse("n(mean = -1)")$strprint(), "Norm(mean = -1, var = 1)")
  expect_equal(dparse("n(var = 8)")$strprint(), "Norm(mean = 0, var = 8)")
  expect_equal(dparse("n(mean=-1,var=1)")$strprint(), "Norm(mean = -1, var = 1)")
})

test_that("Distribution parsing works correctly", {
  expect_s3_class(dparse("T()"), "StudentT")
  expect_s3_class(dparse("LoGNOrmAl(meanlog = 3)"), "Lognormal")
  expect_s3_class(dparse("GaMMa(rate = 3, shape = 8)"), "Gamma")
  expect_s3_class(dparse("C2(df = 2)"), "ChiSquared")
})

test_that("ShortName, ClassName and Alias are unique between distributions", {
  d6 <- listDistributions()[,(ids = paste(tolower(ShortName), tolower(ClassName), tolower(Alias), sep = ", "))]
  d6 <- strsplit(d6, ",")
  # Unique across one class
  d6 <- unlist(lapply(d6, function(x) unique(trimws(x))))
  expect_equal(length(d6), length(unique(d6)))
})

test_that("Every distribution is created with it's propper S3 class", {
  # Get calls
  d6 <- listDistributions()[,(ids = paste(tolower(ShortName), tolower(ClassName), tolower(Alias), sep = ", "))]
  calls <- strsplit(d6, ",")
  calls <- unlist(lapply(calls, function(x) paste0(trimws(x), "()")))
  # Get classes
  counts <- strsplit(d6, ",")
  counts <- unlist(lapply(counts, length))
  counts <- mapply(rep, unlist(listDistributions()[, "ClassName"], use.names = F), each = counts)
  classes <- unlist(counts, use.names = F)
  # Evaluate
  for (i in seq_along(classes)) {
    expect_s3_class(dparse(calls[i]), classes[i])
  }
})
