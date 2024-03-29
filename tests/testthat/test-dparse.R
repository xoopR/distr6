test_that("Function only accepts valid structures", {
  expect_error(dparse("t"), "Call 't' does not have a valid format. See documentation.")
  expect_error(dparse("n("), "Call 'n\\(' does not have a valid format. See documentation.")
  expect_error(dparse("GaMmA)"), "Call 'GaMmA)' does not have a valid format. See documentation.")
  expect_error(dparse("c2)("), "Call 'c2\\)\\(' does not have a valid format. See documentation.")
})

test_that("Parameter parsing works correctly", {
  expect_equal(dparse("n()")$strprint(), "Norm(mean = 0, var = 1)")
  expect_equal(dparse("n(mean = -1)")$strprint(), "Norm(mean = -1, var = 1)")
  expect_equal(dparse("n(var = 8)")$strprint(), "Norm(mean = 0, var = 8)")
  expect_equal(dparse("n(mean=-1,var=1)")$strprint(), "Norm(mean = -1, var = 1)")
})

test_that("Categorical parameters work correctly", {
  expect_equal(dparse("CAT()")$strprint(),
               "Cat(elements = 1, probs = 1)")
  expect_equal(dparse("Cat(elements = c('a'), probs = c(0.3))")$strprint(),
               "Cat(elements = a, probs = 0.3)")
  expect_equal(dparse("Cat(elements = c('a', 'b'), probs = c(0.3, 0.7))")$strprint(),
               "Cat(elements = c(\"a\", \"b\"), probs = c(0.3, 0.7))")
  expect_equal(dparse("Cat(c('a', 'b', 'c'), c(0.3, 0.5, 0.2))")$strprint(),
               "Cat(elements = c(\"a\", \"b\", \"c\"), probs = c(0.3, 0.5, 0.2))")
})

test_that("Distribution parsing works correctly", {
  expect_R6_class(dparse("T()"), "StudentT")
  expect_R6_class(dparse("LoGNOrmAl(meanlog = 3)"), "Lognormal")
  expect_R6_class(dparse("GaMMa(rate = 3, shape = 8)"), "Gamma")
  expect_R6_class(dparse("C2(df = 2)"), "ChiSquared")
})

test_that("ShortName, ClassName and Alias are unique between distributions", {
  d6 <- listDistributions()[,(ids = paste(tolower(ShortName), tolower(ClassName), tolower(Alias), sep = ", "))]
  d6 <- strsplit(d6, ",")
  # Unique across one class
  d6 <- unlist(lapply(d6, function(x) unique(trimws(x))))
  expect_equal(length(d6), length(unique(d6)))
})

test_that("Every distribution is created with it's proper R6 class", {
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
    expect_R6_class(dparse(calls[i]), classes[i])
  }
})

test_that("Aliases are same within distirbution and listDistribution", {
  d6 <- listDistributions()[, c("ClassName", "Alias")]
  for (i in seq_len(nrow(d6))) {
    distr <- eval(parse(text = paste0("distr6::", d6[[i,"ClassName"]], "$new()")))
    expect_equal(distr$alias, d6[[i, "Alias"]])
  }
})
