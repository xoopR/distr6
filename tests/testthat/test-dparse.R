test_that("ShortName, ClassName and Alias are unique across all distributions", {
  d6 <- unlist(listDistributions()[,c("ShortName", "ClassName", "Alias")], use.names = F)
  expect_equal(length(d6), unique(length(d6)))
})

test_that("Every distribution can be created", {
  d6 <- unlist(listDistributions()[,c("ShortName", "ClassName", "Alias")], use.names = F)
  d6 <- paste0(d6, "()")
  expect_invisible(invisible(sapply(d6, dparse)))
})

test_that("Invalid distributions are not accepted", {
  expect_error(dparse("Norm"))
  expect_error(dparse("T("))
  expect_error(dparse("LN)"))
  expect_error(dparse("Ga2ma"))
})
