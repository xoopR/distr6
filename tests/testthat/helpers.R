autotest_sdistribution = function(sdist){
  # fields
  expect_equal(as.character(sdist$inherit), "SDistribution")
  checkmate::expect_names(c(sdist$public_fields$name,
                            sdist$public_fields$short_name,
                            sdist$public_fields$description))
  expect_true(length(strsplit(sdist$public_fields$short_name, " ")[[1]]) == 1)
  if(!is.null(sdist$public_fields$packages)){
    checkmate::expect_names(sdist$public_fields$packages)
  }

  # public methods
  if(!is.null(sdist$public_methods$setParameterValue)){
    expect_equal(names(formals(sdist$public_methods$setParameterValue)), c("...","lst","error"))
  }
  expect_true(all(c("decorators", "verbose") %in% names(formals(sdist$public_methods$initialize))))
  if(!is.null(sdist$public_methods$mean)) expect_null(names(formals(sdist$public_methods$mean)))
  if(!is.null(sdist$public_methods$mode)){
    expect_equal(names(formals(sdist$public_methods$mode)), "which")
    if(!is.null(formals(sdist$public_methods$mode)[[1]])){
      expect_true("all" == formals(sdist$public_methods$mode)[[1]])
    }
  }
  if(!is.null(sdist$public_methods$variance)) expect_null(names(formals(sdist$public_methods$variance)))
  if(!is.null(sdist$public_methods$skewness)) expect_null(names(formals(sdist$public_methods$skewness)))
  if(!is.null(sdist$public_methods$kurtosis)) expect_equal(formals(sdist$public_methods$kurtosis), pairlist(excess = TRUE))
  if(!is.null(sdist$public_methods$entropy)) expect_equal(formals(sdist$public_methods$entropy), pairlist(base = 2))
  if(!is.null(sdist$public_methods$mgf)) expect_equal(names(formals(sdist$public_methods$mgf)), "t")
  if(!is.null(sdist$public_methods$cf)) expect_equal(names(formals(sdist$public_methods$cf)), "t")
  if(!is.null(sdist$public_methods$pgf)) expect_equal(names(formals(sdist$public_methods$pgf)), "z")


  # private methods
  expect_equal(names(formals(sdist$private_methods$.getRefParams)), "paramlst")
  if(!is.null(sdist$private_methods$.pdf)){
    checkmate::expect_subset(names(formals(sdist$private_methods$.pdf)), c("x","log"))
  }
  if(!is.null(sdist$private_methods$.cdf)){
    checkmate::expect_subset(names(formals(sdist$private_methods$.cdf)), c("x","lower.tail", "log.p"))
  }
  if(!is.null(sdist$private_methods$.quantile)){
    checkmate::expect_subset(names(formals(sdist$private_methods$.quantile)), c("p","lower.tail", "log.p"))
  }
  if(!is.null(sdist$private_methods$.rand)){
    expect_equal(names(formals(sdist$private_methods$.rand)), "n")
  }
}
