test_that("ShortName, ClassName and Alias are unique across all distributions", {
  d6 <- unlist(listDistributions()[,c("ShortName", "ClassName", "Alias")], use.names = F)
  expect_equal(length(d6), unique(length(d6)))
})
