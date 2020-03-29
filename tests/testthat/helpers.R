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

  # check no incorrect naming
  expect_null(sdist$private_methods$pdf)
  expect_null(sdist$private_methods$cdf)
  expect_null(sdist$private_methods$quantile)
  expect_null(sdist$private_methods$rand)
}

autotest_kernel = function(kern, shortname, support, variance, squared2Norm, pdf, cdf){
  context("fields")
  expect_equal(as.character(kern$inherit), "Kernel")
  checkmate::expect_names(c(kern$public_fields$name,
                            kern$public_fields$short_name,
                            kern$public_fields$description))
  expect_true(length(strsplit(kern$public_fields$short_name, " ")[[1]]) == 1)
  if(!is.null(kern$public_fields$packages)){
    checkmate::expect_names(kern$public_fields$packages)
  }

  context("public methods")
  if(!is.null(kern$public_methods$setParameterValue)){
    expect_equal(names(formals(kern$public_methods$setParameterValue)), c("...","lst","error"))
  }
  expect_equal(formals(kern$public_methods$initialize), pairlist(decorators = NULL))
  if(!is.null(kern$public_methods$squared2Norm)) expect_null(names(formals(kern$public_methods$squared2Norm)))
  if(!is.null(kern$public_methods$variance)) expect_null(names(formals(kern$public_methods$variance)))
  if(!is.null(kern$public_methods$skewness)) expect_null(names(formals(kern$public_methods$skewness)))
  if(!is.null(kern$public_methods$kurtosis)) expect_equal(formals(kern$public_methods$kurtosis), pairlist(excess = TRUE))
  if(!is.null(kern$public_methods$entropy)) expect_equal(formals(kern$public_methods$entropy), pairlist(base = 2))
  if(!is.null(kern$public_methods$mgf)) expect_equal(names(formals(kern$public_methods$mgf)), "t")
  if(!is.null(kern$public_methods$cf)) expect_equal(names(formals(kern$public_methods$cf)), "t")
  if(!is.null(kern$public_methods$pgf)) expect_equal(names(formals(kern$public_methods$pgf)), "z")


  context("private methods")
  checkmate::expect_subset(names(formals(kern$private_methods$.pdf)), c("x","log"))
  if(!is.null(kern$private_methods$.cdf)){
    checkmate::expect_subset(names(formals(kern$private_methods$.cdf)), c("x","lower.tail", "log.p"))
  }
  if(!is.null(kern$private_methods$.quantile)){
    checkmate::expect_subset(names(formals(kern$private_methods$.quantile)), c("p","lower.tail", "log.p"))
  }

  # check no incorrect naming
  expect_null(kern$private_methods$pdf)
  expect_null(kern$private_methods$cdf)
  expect_null(kern$private_methods$quantile)
  expect_null(kern$private_methods$rand)

  # kernel specific tests
  kern = kern$new()
  expect_equal(kern$mean(), 0)
  expect_equal(kern$median(), 0)
  expect_equal(kern$mode(), 0)
  expect_equal(kern$properties$support$strprint(), support$strprint())
  expect_equal(kern$variance(), variance)
  expect_equal(kern$squared2Norm(), squared2Norm)
  expect_equal(kern$strprint(), shortname)
  expect_output(kern$summary())
  expect_output(kern$summary(F))
  expect_equal(round(kern$pdf(c(-0.1,0,0.1)),4), pdf)
  if(!is.null(kern$private_methods$.cdf)){
    expect_equal(round(kern$cdf(c(-0.1,0,0.1)),4), cdf)
  }
  if(!is.null(kern$private_methods$.quantile)){
    expect_equal(kern$quantile(kern$cdf(c(-0.42,0.24,0.42))), c(-0.42,0.24,0.42))
    expect_equal(length(kern$rand(1:3)), 3)
    checkmate::expect_data_table(kern$rand(1:3,simplify = F), ncols = 3, nrows = 1)
  }

}

