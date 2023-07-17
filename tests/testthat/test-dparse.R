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
  calls <- unlist(lapply(calls, function(x) trimws(x)))
  calls <- calls[calls != ""]
  calls <- paste0(calls, "()")
  # Get classes
  counts <- strsplit(d6, ",")
  counts <- unlist(lapply(counts, length))
  # Fix for Exponential distribution where Alias = "";
  counts[16] <- counts[16] - 1
  counts <- mapply(rep, unlist(listDistributions()[, "ClassName"], use.names = F), each = counts)
  classes <- unlist(counts, use.names = F)
  # Evaluate
  for (i in seq_along(classes)) {
    expect_s3_class(dparse(calls[i]), classes[i])
  }
})

test_that("Invalid distributions are not accepted", {
  expect_error(dparse("Norm"))
  expect_error(dparse("T("))
  expect_error(dparse("LN)"))
  expect_error(dparse("Ga2ma"))
})
