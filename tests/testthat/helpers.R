expect_rounded_equal <- function(object, expected, dp = 4) {
  expect_equal(round(object, dp), round(expected, dp),
    label = as.character(substitute(quote(object)))[2],
    expected.label = as.character(substitute(quote(expected)))[2]
  )
}

autotest_sdistribution <- function(sdist, pars, traits, support, symmetry,
                                   mean = NULL, mode = NULL, median = NULL,
                                   variance = NULL, skewness = NULL, exkur = NULL, entropy = NULL,
                                   mgf = NULL, cf = NULL,
                                   pgf = NULL, pdf = NULL, cdf = NULL, quantile = NULL,
                                   vectorise = TRUE) {

  # context("public fields")
  checkmate::assertSubset(names(sdist$public_fields), c("name", "short_name", "description",
                                                        "packages"))
  expect_equal(as.character(sdist$inherit), "SDistribution")
  checkmate::expect_names(c(
    sdist$public_fields$name,
    sdist$public_fields$short_name,
    sdist$public_fields$description
  ))
  expect_true(length(strsplit(sdist$public_fields$short_name, " ")[[1]]) == 1)
  if (!is.null(sdist$public_fields$packages)) {
    checkmate::expect_names(sdist$public_fields$packages)
  }

  # context("public methods")
  checkmate::expect_subset(
    names(sdist$public_methods),
    c(
      "clone", "mean", "mode", "variance", "skewness", "kurtosis",
      "entropy", "mgf", "cf", "pgf", "setParameterValue", "initialize",
      "median", "getParameterValue", "strprint"
    )
  )

  if (!is.null(sdist$public_methods$setParameterValue)) {
    expect_equal(names(formals(sdist$public_methods$setParameterValue)), c("...", "lst", "error"))
  }
  expect_true("decorators" %in% names(formals(sdist$public_methods$initialize)))
  if (!is.null(sdist$public_methods$mean))
    expect_equal(names(formals(sdist$public_methods$mean)), "...")
  if (!is.null(sdist$public_methods$mode))
    expect_equal(formals(sdist$public_methods$mode), pairlist(which = "all"))
  if (!is.null(sdist$public_methods$median))
    expect_null(names(formals(sdist$public_methods$median)))
  if (!is.null(sdist$public_methods$variance))
    expect_equal(names(formals(sdist$public_methods$variance)), "...")
  if (!is.null(sdist$public_methods$skewness))
    expect_equal(names(formals(sdist$public_methods$skewness)), "...")
  if (!is.null(sdist$public_methods$kurtosis))
    expect_equal(formals(sdist$public_methods$kurtosis),
                 as.pairlist(c(pairlist(excess = TRUE), alist(... = ))))
  if (!is.null(sdist$public_methods$entropy))
    expect_equal(formals(sdist$public_methods$entropy),
                 as.pairlist(c(pairlist(base = 2), alist(... = ))))
  if (!is.null(sdist$public_methods$mgf))
    expect_equal(names(formals(sdist$public_methods$mgf)), c("t", "..."))
  if (!is.null(sdist$public_methods$cf))
    expect_equal(names(formals(sdist$public_methods$cf)), c("t", "..."))
  if (!is.null(sdist$public_methods$pgf))
    expect_equal(names(formals(sdist$public_methods$pgf)), c("z", "..."))

  # context("private methods")
  checkmate::expect_subset(names(sdist$private_methods), c(
    ".pdf", ".cdf", ".quantile", ".rand",
    ".log"
  ))
  if (!is.null(sdist$private_methods$.log)) checkmate::expect_flag(sdist$private_methods$.log)

  checkmate::expect_subset(names(formals(sdist$private_methods$.pdf)), c("x", "log"))
  if (!is.null(sdist$private_methods$.cdf))
    checkmate::expect_subset(names(formals(sdist$private_methods$.cdf)),
                             c("x", "lower.tail", "log.p"))
  if (!is.null(sdist$private_methods$.quantile))
    checkmate::expect_subset(names(formals(sdist$private_methods$.quantile)),
                             c("p", "lower.tail", "log.p"))
  if (!is.null(sdist$private_methods$.rand))
    expect_equal(formals(sdist$private_methods$.rand), as.pairlist(alist(n = ))) # nolint

  sdist <- expect_silent({
    do.call(sdist$new, pars)
  })


  if (vectorise) {
    autotest_vec_sdistribution(sdist, pars)
  }

  # context("sdist specific - properties & traits")
  expect_equal(sdist$traits, traits)
  expect_equal(sdist$properties$support, support)
  expect_equal(sdist$properties$symmetry, symmetry)

  # context("sdist specific - public methods")
  if (!is.null(sdist$mean)) {
    expect_rounded_equal(sdist$mean(), mean, 4)
  }
  if (!is.null(sdist$mode)) {
    expect_rounded_equal(sdist$mode(), mode, 4)
  }
  if (!is.null(sdist$median)) {
    if (is.null(median)) {
      expect_equal(sdist$median(), median)
    } else {
      expect_rounded_equal(sdist$median(), median, 4)
    }
  }
  if (!is.null(sdist$variance)) {
    expect_rounded_equal(sdist$variance(), variance, 4)
  }
  if (!is.null(sdist$skewness)) {
    expect_rounded_equal(sdist$skewness(), skewness, 4)
  }
  if (!is.null(sdist$kurtosis)) {
    expect_rounded_equal(sdist$kurtosis(T), exkur, 3)
    expect_rounded_equal(sdist$kurtosis(F), exkur + 3, 3)
  }
  if (!is.null(sdist$entropy)) {
    expect_rounded_equal(sdist$entropy(), entropy, 4)
  }
  if (testUnivariate(sdist)) {
    if (!is.null(sdist$mgf)) {
      expect_rounded_equal(sdist$mgf(1), mgf, 4)
    }
    if (!is.null(sdist$cf)) {
      expect_rounded_equal(sdist$cf(1), cf, 4)
    }
    if (!is.null(sdist$pgf)) {
      expect_rounded_equal(sdist$pgf(1), pgf, 4)
    }
  } else {
    if (!is.null(sdist$mgf)) expect_rounded_equal(sdist$mgf(1:2), mgf, 4)
    if (!is.null(sdist$cf)) expect_rounded_equal(sdist$cf(1:2), cf, 4)
    if (!is.null(sdist$pgf)) expect_rounded_equal(sdist$pgf(1:2), pgf, 4)
  }



  # context("sdist specific - representation methods")
  expect_output(sdist$print())
  expect_output(sdist$summary())
  expect_output(sdist$summary(F))

  # context("d/p/q/r")
  if (testUnivariate(sdist)) {
    if (isPdf(sdist)) {
      expect_rounded_equal(sdist$pdf(1:3), pdf)
      if (sdist$.__enclos_env__$private$.log) {
        expect_rounded_equal(sdist$pdf(1:3, log = TRUE), log(pdf))
      }
    }
    if (isCdf(sdist)) {
      expect_rounded_equal(sdist$cdf(1:3), cdf)
      if (sdist$.__enclos_env__$private$.log) {
        expect_rounded_equal(sdist$cdf(1:3, lower.tail = FALSE, log.p = TRUE), log(1 - cdf), 3)
      }
    }
    if (isQuantile(sdist)) {
      expect_rounded_equal(sdist$quantile(c(0.24, 0.42, 0.5)), quantile)
      if (sdist$.__enclos_env__$private$.log) {
        expect_rounded_equal(sdist$quantile(log(1 - c(0.24, 0.42, 0.5)),
                                            lower.tail = FALSE, log.p = TRUE), quantile)
      }
    }
    if (isRand(sdist)) {
      r <- sdist$rand(1:3)
      expect_equal(length(r), 3)
      expect_true(all(r >= sdist$inf & r <= sdist$sup))
    }
  } else {
    if (isRand(sdist)) {
      r <- sdist$rand(1:2)
      expect_equal(dim(r), c(2, 2))
      expect_true(all(sdist$cdf(data = r) > 0))
    }
  }

}

create_named_vector <- function(v, n) {
  names(v) <- n
  return(v)
}

test_vectorised_method <- function(vdist, method, args = NULL) {
  if (is.null(args)) {
    expect_equal(vdist[[method]](),
                 create_named_vector(c(vdist[1][[method]](), vdist[2][[method]](),
                                       vdist[3][[method]]()),
                                     as.character(unlist(vdist$modelTable$shortname))))
  } else {
    expect_equal(vdist[[method]](args),
                 create_named_vector(c(vdist[1][[method]](args), vdist[2][[method]](args),
                                       vdist[3][[method]](args)),
                                     as.character(unlist(vdist$modelTable$shortname))))
  }
}

test_vectorised_mv_method <- function(vdist, method, args = NULL) {
  if (method == "variance") {
    dt <- array(c(vdist[1]$variance(), vdist[2]$variance(), vdist[3]$variance()),
                dim = c(2, 2, 3), dimnames = list(NULL, NULL,
                                                  as.character(unlist(vdist$modelTable$shortname))))
    expect_equal(vdist$variance(), dt)
  } else if (method %in% c("entropy", "skewness", "kurtosis")) {
    if (is.null(args)) {
      dt <- c(vdist[1][[method]](), vdist[2][[method]](),
                       vdist[3][[method]]())
      names(dt) <- as.character(unlist(vdist$modelTable$shortname))
      expect_equal(vdist[[method]](), dt)
    } else {
      dt <- c(vdist[1][[method]](args), vdist[2][[method]](args),
                       vdist[3][[method]](args))
      names(dt) <- as.character(unlist(vdist$modelTable$shortname))
      expect_equal(vdist[[method]](args), dt)
    }
  } else {
    if (is.null(args)) {
      dt <- data.table(vdist[1][[method]](), vdist[2][[method]](),
              vdist[3][[method]]())
      colnames(dt) <- as.character(unlist(vdist$modelTable$shortname))
      expect_equal(vdist[[method]](), dt)
    } else {
      dt <- data.table(vdist[1][[method]](args), vdist[2][[method]](args),
              vdist[3][[method]](args))
      colnames(dt) <- as.character(unlist(vdist$modelTable$shortname))
      expect_equal(vdist[[method]](args), dt)
    }
  }
}

test_vectorised_mv_dpqr <- function(vdist, method, args = NULL) {
  expected <- data.table::data.table(do.call(vdist[1][[method]], args),
                                     do.call(vdist[2][[method]], args),
                                     do.call(vdist[3][[method]], args))
  colnames(expected) <- as.character(unlist(vdist$modelTable$shortname))
  object <- do.call(vdist[[method]], args)
  expect_equal(object, expected)
}


test_vectorised_dpqr <- function(vdist, method, args = NULL) {
  expected <- data.table::data.table(do.call(vdist[1][[method]], args),
                                     do.call(vdist[2][[method]], args),
                                     do.call(vdist[3][[method]], args))
  colnames(expected) <- as.character(unlist(vdist$modelTable$shortname))
  object <- do.call(vdist[[method]], args)
  expect_equal(object, expected)
}

autotest_vec_sdistribution <- function(sdist, pars) {
  if (testMultivariate(sdist)) {
    autotest_vec_mv_sdistribution(sdist, pars)
  } else {
    if (sdist$name == "Geometric" & !is.null(pars$trials)) {
      vdist <- VectorDistribution$new(distribution = sdist$name,
                                      params = rep(list(pars[!(names(pars) %in% "trials")]), 3),
                                      shared_params = list(trials = pars$trials))
    } else if (sdist$name == "NegativeBinomial" & !is.null(pars$form)) {
      vdist <- VectorDistribution$new(distribution = sdist$name,
                                      params = rep(list(pars[!(names(pars) %in% "form")]), 3),
                                      shared_params = list(form = pars$form))
    } else {
      vdist <- VectorDistribution$new(distribution = sdist$name,
                                      params = rep(list(pars), 3))
    }


    if (!is.null(sdist$mean)) test_vectorised_method(vdist, "mean")
    if (!is.null(sdist$mode)) {
      # hacky catch
      if (sdist$name == "Categorical") {
        expect_equal(vdist$mode(1),
                     list(Cat1 = vdist[1]$mode(1),
                          Cat2 = vdist[1]$mode(1),
                          Cat3 = vdist[1]$mode(1))
        )
      } else {
        test_vectorised_method(vdist, "mode", 1)
      }
    }
    # if (!is.null(sdist$median)) test_vectorised_method(vdist, "median")
    if (!is.null(sdist$variance)) test_vectorised_method(vdist, "variance")
    if (!is.null(sdist$skewness)) test_vectorised_method(vdist, "skewness")
    if (!is.null(sdist$kurtosis)) test_vectorised_method(vdist, "kurtosis")
    if (!is.null(sdist$entropy)) test_vectorised_method(vdist, "entropy")
    # if (testUnivariate(sdist)) {
    #   if (!is.null(sdist$mgf)) suppressWarnings(test_vectorised_method(vdist, "mgf", 1)
    #   if (!is.null(sdist$cf)) test_vectorised_method(vdist, "cf", 1)
    #   if (!is.null(sdist$pgf)) test_vectorised_method(vdist, "pgf", 1)
    # } else {
    #   if (!is.null(sdist$mgf)) test_vectorised_method(vdist, "mgf", 1:2)
    #   if (!is.null(sdist$cf)) test_vectorised_method(vdist, "cf", 1:2)
    #   if (!is.null(sdist$pgf)) test_vectorised_method(vdist, "pgf", 1:2)
    # }

    # context("d/p/q/r")
      if (isPdf(sdist)) {
        test_vectorised_dpqr(vdist, "pdf", list(1:3))
        if (sdist$.__enclos_env__$private$.log) {
          test_vectorised_dpqr(vdist, "pdf", list(1:3, log = TRUE))
        }
      }
      if (isCdf(sdist)) {
        test_vectorised_dpqr(vdist, "cdf", list(1:3))
        if (sdist$.__enclos_env__$private$.log) {
          test_vectorised_dpqr(vdist, "cdf", list(1:3, log.p = TRUE, lower.tail = FALSE))
        }
      }
      if (isQuantile(sdist)) {
        test_vectorised_dpqr(vdist, "quantile", list(c(0.24, 0.42, 0.5)))
        if (sdist$.__enclos_env__$private$.log) {
          test_vectorised_dpqr(vdist, "quantile", list(log(1 - c(0.24, 0.42, 0.5)),
                                                       log.p = TRUE, lower.tail = FALSE))
        }
      }
      if (isRand(sdist)) {
        r <- vdist$rand(1:4)
        expect_equal(dim(r), c(4, 3))
        expect_true(all(as.numeric(unlist(r)) >= sdist$inf & as.numeric(unlist(r)) <= sdist$sup))
      }
    }
}

autotest_vec_mv_sdistribution <- function(sdist, pars) {
  vdist <- VectorDistribution$new(distribution = sdist$name,
                                  params = rep(list(pars), 3))


  if (!is.null(sdist$mean)) test_vectorised_mv_method(vdist, "mean")
  if (!is.null(sdist$mode)) test_vectorised_mv_method(vdist, "mode", 1)

  # if (!is.null(sdist$median)) test_vectorised_method(vdist, "median")
  if (!is.null(sdist$variance)) test_vectorised_mv_method(vdist, "variance")
  if (!is.null(sdist$skewness)) test_vectorised_mv_method(vdist, "skewness")
  if (!is.null(sdist$kurtosis)) test_vectorised_mv_method(vdist, "kurtosis")
  if (!is.null(sdist$entropy)) test_vectorised_mv_method(vdist, "entropy")

    if (isPdf(sdist)) {
      if (sdist$name == "Dirichlet") {
        data <- data.frame(c(0.1, 0.2, 0.3), c(0.9, 0.8, 0.7))
      } else {
        data <- data.frame(1:3, 1:3)
      }
      test_vectorised_mv_dpqr(vdist, "pdf", list(data = data))
      if (sdist$.__enclos_env__$private$.log) {
        test_vectorised_mv_dpqr(vdist, "pdf", list(data = data, log = TRUE))
      }
    }
    if (isCdf(sdist)) {
      test_vectorised_mv_dpqr(vdist, "cdf", list(data = data))
      if (sdist$.__enclos_env__$private$.log) {
        test_vectorised_mv_dpqr(vdist, "cdf", list(data = data,
                                                   log.p = TRUE, lower.tail = FALSE))
      }
    }
    if (isRand(sdist)) {
      r <- vdist$rand(1:4)
      expect_equal(dimnames(r), list(NULL, c("V1", "V2"),
                                     as.character(unlist(vdist$modelTable$shortname))))
      expect_true(all(as.numeric(r) >= sdist$inf$elements[[1]] &
                        as.numeric(r) <= sdist$sup$elements[[2]]))
    }
}

autotest_kernel <- function(kern, shortname, support, variance, pdfSquared2Norm, pdf, cdf) {
  # context("public fields")
  checkmate::expect_subset(names(kern$public_fields), c("name", "short_name", "description",
                                                        "packages"))
  expect_equal(as.character(kern$inherit), "Kernel")
  checkmate::expect_names(c(
    kern$public_fields$name,
    kern$public_fields$short_name,
    kern$public_fields$description
  ))
  expect_true(length(strsplit(kern$public_fields$short_name, " ")[[1]]) == 1)
  if (!is.null(kern$public_fields$packages)) {
    checkmate::expect_names(kern$public_fields$packages)
  }

  # context("public methods")
  checkmate::expect_subset(names(kern$public_methods), c("clone", "pdfSquared2Norm",
                                                         "cdfSquared2Norm", "variance",
                                                         "initialize"))

  if (!is.null(kern$public_methods$setParameterValue)) {
    expect_equal(names(formals(kern$public_methods$setParameterValue)), c("...", "lst", "error"))
  }
  # expect_equal(formals(kern$public_methods$initialize), pairlist(decorators = NULL))
  if (!is.null(kern$public_methods$pdfSquared2Norm)) {
    expect_equal(formals(kern$public_methods$pdfSquared2Norm), pairlist(x = 0, upper = Inf))
  }
  if (!is.null(kern$public_methods$cdfSquared2Norm)) {
    expect_equal(formals(kern$public_methods$cdfSquared2Norm), pairlist(x = 0, upper = 0))
  }
  if (!is.null(kern$public_methods$variance))
    expect_equal(names(formals(kern$public_methods$variance)), "...")

  checkmate::expect_subset(names(kern$private_methods), c(".pdf", ".cdf", ".quantile", ".rand"))

  checkmate::expect_subset(names(formals(kern$private_methods$.pdf)), c("x", "log"))
  if (!is.null(kern$private_methods$.cdf)) {
    checkmate::expect_subset(names(formals(kern$private_methods$.cdf)), c("x", "lower.tail",
                                                                          "log.p"))
  }
  if (!is.null(kern$private_methods$.quantile)) {
    checkmate::expect_subset(names(formals(kern$private_methods$.quantile)), c("p", "lower.tail",
                                                                               "log.p"))
  }

  # context("kernel specific")
  kern <- kern$new()
  expect_equal(kern$mean(), 0)
  expect_equal(kern$median(), 0)
  expect_equal(kern$mode(), 0)
  expect_equal(kern$properties$support$strprint(), support$strprint())
  expect_equal(kern$variance(), variance)
  expect_rounded_equal(kern$pdfSquared2Norm(c(0, 1, 3)), pdfSquared2Norm)
  expect_equal(kern$strprint(), paste0(shortname, "()"))
  expect_output(kern$summary())
  expect_output(kern$summary(F))

  # context("d/p/q/r")
  if (isPdf(kern)) {
    expect_rounded_equal(kern$pdf(c(-0.1, 0, 0.1)), pdf, 2)
    if (kern$.__enclos_env__$private$.log) {
      expect_rounded_equal(kern$pdf(c(-0.1, 0, 0.1), log = TRUE), log(pdf), 2)
    }
  }
  if (isCdf(kern)) {
    expect_rounded_equal(kern$cdf(c(-0.1, 0, 0.1)), cdf, 3)
    if (kern$.__enclos_env__$private$.log) {
      expect_rounded_equal(kern$cdf(c(-0.1, 0, 0.1), lower.tail = FALSE, log.p = TRUE),
                           log(1 - cdf), 3)
    }
  }

  if (isQuantile(kern)) {
    expect_rounded_equal(kern$quantile(kern$cdf(c(-0.42, 0.24, 0.42))), c(-0.42, 0.24, 0.42), 2)
    expect_equal(length(kern$rand(1:3)), 3)
    checkmate::expect_data_table(kern$rand(1:3, simplify = F), nrows = 3, ncols = 1)
    if (kern$.__enclos_env__$private$.log) {
      expect_rounded_equal(kern$quantile(kern$cdf(c(-0.42, 0.24, 0.42), lower.tail = FALSE,
                                                  log.p = TRUE), lower.tail = FALSE, log.p = TRUE),
                           c(-0.42, 0.24, 0.42), 3)
    }
  }
}

expect_equal_distribution <- function(d1, d2) {
  expect_equal(class(d1), class(d2))

  expect_equal(d1$traits$variateForm, d2$traits$variateForm)
  expect_equal(d1$traits$valueSupport, d2$traits$valueSupport)
  expect_equal(d1$traits$type$strprint(), d2$traits$type$strprint())

  expect_equal(d1$properties$support$strprint(), d2$properties$support$strprint())
  expect_equal(d1$properties$symmetric, d2$properties$symmetric)

  expect_equal(d1$name, d2$name)
  expect_equal(d1$short_name, d2$short_name)
  expect_equal(d1$description, d2$description)
  expect_equal(d1$pdf(1:5), d2$pdf(1:5))
  expect_equal(d1$cdf(1:5), d2$cdf(1:5))
  q = try(d1$quantile(0.1), silent = TRUE)
  if (class(q)[1] != "try-error")
    expect_equal(d1$quantile(c(0.1,0.2,0.3)), d2$pdf(c(0.1,0.2,0.3)))

  p1 <- as.data.table(d1$parameters())
  p1$support <- rsapply(p1$support, "strprint")
  p2 <- as.data.table(d2$parameters())
  p2$support <- rsapply(p2$support, "strprint")
  expect_equal(p1, p2)
}
