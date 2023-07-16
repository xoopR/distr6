test_that("ShortName, ClassName and Shortcut are unique across all distributions", {
  d6 <- unlist(listDistributions()[,c("ShortName", "ClassName", "Shortcut")], use.names = F)
  expect_equal(length(d6), unique(length(d6)))
})
