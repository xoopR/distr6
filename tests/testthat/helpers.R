autotest_sdistribution = function(sdist, pars, traits, support, symmetry, mean, mode,
                                  variance, skewness, exkur, entropy, mgf, cf,
                                  pgf, pdf, cdf){

  context("public fields")
  expect_equal(names(sdist$public_fields), c("name", "short_name", "description", "packages"))
  expect_equal(as.character(sdist$inherit), "SDistribution")
  checkmate::expect_names(c(sdist$public_fields$name,
                            sdist$public_fields$short_name,
                            sdist$public_fields$description))
  expect_true(length(strsplit(sdist$public_fields$short_name, " ")[[1]]) == 1)
  if(!is.null(sdist$public_fields$packages)){
    checkmate::expect_names(sdist$public_fields$packages)
  }

  context("public methods")
  checkmate::expect_subset(names(sdist$public_methods),
                           c("clone", "mean", "mode", "variance", "skewness", "kurtosis",
                             "entropy", "mgf", "cf", "pgf", "setParameterValue", "initialize"))

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

  context("private methods")
  checkmate::expect_subset(names(sdist$private_methods), c(".pdf", ".cdf", ".quantile", ".rand", ".getRefParams"))

  checkmate::expect_subset(names(formals(sdist$private_methods$.pdf)), c("x","log"))
  if(!is.null(sdist$private_methods$.cdf))  checkmate::expect_subset(names(formals(sdist$private_methods$.cdf)), c("x","lower.tail", "log.p"))
  if(!is.null(sdist$private_methods$.quantile)) checkmate::expect_subset(names(formals(sdist$private_methods$.quantile)), c("p","lower.tail", "log.p"))
  if(!is.null(sdist$private_methods$.rand)) expect_equal(formals(sdist$private_methods$.rand), as.pairlist(alist(n = )))
  if(!is.null(sdist$private_methods$.getRefParams)) expect_equal(formals(sdist$private_methods$.getRefParams), as.pairlist(alist(paramlst = )))


  expect_message(do.call(sdist$new, c(pars, list(verbose = TRUE))))
  expect_silent(sdist$new())
  sdist = expect_silent({do.call(sdist$new, pars)})

  context("sdist specific - properties & traits")
  expect_equal(sdist$traits, traits)
  expect_equal(sdist$properties$support, support)
  expect_equal(sdist$properties$symmetry, symmetry)

  context("sdist specific - public methods")
  expect_equal(sdist$mean(), mean)
  expect_equal(sdist$mode(), mode)
  expect_equal(sdist$variance(), variance)
  expect_equal(sdist$skewness(), skewness)
  expect_equal(sdist$kurtosis(T), exkur)
  expect_equal(sdist$kurtosis(F), exkur + 3)
  expect_equal(round(sdist$entropy(), 4), entropy)
  expect_equal(round(sdist$mgf(1), 4), mgf)
  expect_equal(round(sdist$cf(1), 4), cf)
  expect_equal(round(sdist$pgf(1), 4), pgf)

  context("sdist specific - representation methods")
  expect_output(sdist$print())
  expect_output(sdist$summary())
  expect_output(sdist$summary(F))

  context("d/p/q/r")
  if(!is.null(sdist$private_methods$.pdf) & !missing(pdf)){
    expect_equal(round(sdist$pdf(1:3),4), pdf)
  }
  if(!is.null(sdist$private_methods$.cdf) & !missing(cdf)){
    expect_equal(round(sdist$cdf(1:3),4), cdf)
  }
  if(!is.null(sdist$private_methods$.quantile)){
    expect_equal(sdist$quantile(sdist$cdf(1:10)), 1:10)
  }
  if(!is.null(sdist$private_methods$.rand)){
    r = sdist$rand(1:3)
    expect_equal(length(r), 3)
    epect_true(all(r >= sdist$inf & r <= sdist$sup))
  }
}

autotest_kernel = function(kern, shortname, support, variance, squared2Norm, pdf, cdf){
  context("public fields")
  expect_equal(names(kern$public_fields), c("name", "short_name", "description"))
  expect_equal(as.character(kern$inherit), "Kernel")
  checkmate::expect_names(c(kern$public_fields$name,
                            kern$public_fields$short_name,
                            kern$public_fields$description))
  expect_true(length(strsplit(kern$public_fields$short_name, " ")[[1]]) == 1)
  if(!is.null(kern$public_fields$packages)){
    checkmate::expect_names(kern$public_fields$packages)
  }

  context("public methods")
  checkmate::expect_subset(names(kern$public_methods), c("clone", "squared2Norm", "variance", "initialize"))

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
  checkmate::expect_subset(names(kern$private_methods), c(".pdf", ".cdf", ".quantile", ".rand"))

  checkmate::expect_subset(names(formals(kern$private_methods$.pdf)), c("x","log"))
  if(!is.null(kern$private_methods$.cdf)){
    checkmate::expect_subset(names(formals(kern$private_methods$.cdf)), c("x","lower.tail", "log.p"))
  }
  if(!is.null(kern$private_methods$.quantile)){
    checkmate::expect_subset(names(formals(kern$private_methods$.quantile)), c("p","lower.tail", "log.p"))
  }

  context("kernel specific")
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

  context("d/p/q/r")
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

