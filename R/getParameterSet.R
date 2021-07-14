getParameterSet <- function(object, ...) {
  UseMethod("getParameterSet", object)
}

getParameterSet.Arcsine <- function(object, ...) {
  pset(
    prm("lower", "reals", 0),
    prm("upper", "reals", 1),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("leq", id = "upper"))
    )
  )
}

getParameterSet.Bernoulli <- function(object, ...) {
  pset(
    prm("prob", Interval$new(0, 1), 0.5, "probs"),
    prm("qprob", Interval$new(0, 1), tags = "probs"),
    tag_properties = list(linked = "probs"),
    trafo = function(x, self) {
      if (!is.null(x$qprob)) {
        x$prob <- 1 - x$qprob
      } else if (!is.null(x$prob)) {
        x$qprob <- 1 - x$prob
      }
      x
    }
  )
}

getParameterSet.Beta <- function(object, ...) {
  pset(
    prm("shape1", "posreals", 1),
    prm("shape2", "posreals", 1)
  )
}

getParameterSet.BetaNoncentral <- function(object, ...) {
  pset(
    prm("shape1", "posreals", 1),
    prm("shape2", "posreals", 1),
    prm("location", "posreals0", 1)
  )
}

getParameterSet.Binomial <- function(object, ...) {
  pset(
    prm("prob", Interval$new(0, 1), 0.5, "probs"),
    prm("qprob", Interval$new(0, 1), tags = "probs"),
    prm("size", "posnaturals", 10),
    tag_properties = list(linked = "probs"),
    trafo = function(x, self) {
      if (!is.null(x$qprob)) {
        x$prob <- 1 - x$qprob
      } else if (!is.null(x$prob)) {
        x$qprob <- 1 - x$prob
      }
      x
    }
  )
}

getParameterSet.Categorical <- function(object, ...) {
  pset(
    prm("elements", "universal", 1),
    prm("probs", Interval$new(0, 1)^"n", 1),
    deps = list(
      list(id = "probs", on = "elements", cnd = cnd("len", id = "elements"))
    ),
    trafo = function(x, self) {
      if (!is.null(x$probs)) {
        x$probs <- x$probs / sum(x$probs)
      }
      x
    }
  )
}

getParameterSet.Cauchy <- function(object, ...) {
  pset(
    prm("location", "reals", 0),
    prm("scale", "posreals", 1)
  )
}

getParameterSet.ChiSquared <- function(object, ...) {
  pset(
    prm("df", "posreals0", 1)
  )
}

getParameterSet.ChiSquaredNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df", "posreals0", 1),
    prm("location", "posreals0", 0)
  )
}

getParameterSet.Degenerate <- function(object, ...) {
  pset(
    prm("mean", "reals", 0)
  )
}

getParameterSet.Dirichlet <- function(object, ...) {
  pset(
    prm("params", "nposreals", rep(1, 2))
  )
}

getParameterSet.DiscreteUniform <- function(object, ...) { # nolint
  pset(
    prm("lower", "integers", 0),
    prm("upper", "integers", 1),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("lt", id = "upper"))
    )
  )
}

getParameterSet.Erlang <- function(object, ...) {
  pset(
    prm("shape", "posintegers", 1),
    prm("rate", "posreals", 1, "scales"),
    prm("scale", "posreals", tags = "scales"),
    tag_properties = list(linked = "scales"),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$rate <- 1 / x$scale
      } else if (!is.null(x$rate)) {
        x$scale <- 1 / x$rate
      }
      x
    }
  )
}

getParameterSet.Exponential <- function(object, ...) {
  pset(
    prm("rate", "posreals", 1, "scales"),
    prm("scale", "posreals", tags = "scales"),
    tag_properties = list(linked = "scales"),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$rate <- 1 / x$scale
      } else if (!is.null(x$rate)) {
        x$scale <- 1 / x$rate
      }
      x
    }
  )
}

getParameterSet.FDistribution <- function(object, ...) {
  pset(
    prm("df1", "posreals", 1),
    prm("df2", "posreals", 1)
  )
}

getParameterSet.FDistributionNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df1", "posreals", 1),
    prm("df2", "posreals", 1),
    prm("location", "posreals0", 0)
  )
}

getParameterSet.Frechet <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("scale", "posreals", 1),
    prm("minimum", "reals", 0)
  )
}

getParameterSet.Gamma <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("rate", "posreals", 1, "scales"),
    prm("scale", "posreals", tags = "scales"),
    prm("mean", "posreals", tags = "scales"),
    tag_properties = list(linked = "scales"),
    trafo = function(x, self) {
      if (!is.null(x$rate)) {
        x$scale <- 1 / x$rate
        x$mean <- x$shape / x$rate
      } else if (!is.null(x$scale)) {
        x$rate <- 1 / x$scale
        x$mean <- x$shape * x$scale
      } else if (!is.null(x$mean)) {
        x$rate <- x$shape / x$mean
        x$scale <- 1 / x$rate
      }
      x
    }
  )
}

getParameterSet.Geometric <- function(object, trials = FALSE, ...) {

  if (trials) {
    ps <- ParameterSet$new(
      id = list("prob", "qprob", "trials"),
      value = list(0.5, 0.5, TRUE),
      support = list(
        Interval$new(0, 1, type = "()"),
        Interval$new(0, 1, type = "()"),
        Logicals$new()
      ),
      settable = list(TRUE, TRUE, FALSE),
      description = list(
        "Probability of success",
        "Probability of failure",
        "Form: number of trials before first success"
      )
    )
  } else {
    ps <- ParameterSet$new(
      id = list("prob", "qprob", "trials"),
      value = list(0.5, 0.5, FALSE),
      support = list(
        Interval$new(0, 1, type = "(]"),
        Interval$new(0, 1, type = "(]"),
        Logicals$new()
      ),
      settable = list(TRUE, TRUE, FALSE),
      description = list(
        "Probability of success",
        "Probability of failure",
        "Form: number of failures before first success"
      )
    )
  }

  ps$addDeps("prob", "qprob", function(self) list(qprob = 1 - self$getParameterValue("prob")))
  ps$addDeps("qprob", "prob", function(self) list(prob = 1 - self$getParameterValue("qprob")))

  return(ps)
}

getParameterSet.Gompertz <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("scale", "posreals", 1)
  )
}

getParameterSet.Gumbel <- function(object, ...) {
  pset(
    prm("location", "reals", 0),
    prm("scale", "posreals", 1)
  )
}

getParameterSet.Hypergeometric <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("size", "successes", "failures", "draws"),
    value = list(50, 5, 45, 10),
    support = list(naturals,
                  Set$new(0:50, class = "integer"),
                  Set$new(0:50, class = "integer"),
                  Set$new(0:50, class = "integer")),
    description = list(
      "Population size",
      "Number of successes in the population.",
      "Number of failures in the population.",
      "Number of draws."
    )
  )

  ps$addDeps(
    "successes", "failures", function(self) {
      list(failures = self$getParameterValue("size") - self$getParameterValue("successes"))
    })
  ps$addDeps(
    "failures", "successes", function(self) {
      list(successes = self$getParameterValue("size") - self$getParameterValue("failures"))
    })
  ps$addChecks(function(self) {
    successes <- unlist(self$getParameterValue("successes"))
    failures <- unlist(self$getParameterValue("failures"))
    size <- unlist(self$getParameterValue("size"))
    all(successes <= size) && all(failures <= size)
  })

  return(ps)

}

getParameterSet.InverseGamma <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("scale", "posreals", 1)
  )
}

getParameterSet.Laplace <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("mean", "scale", "var"), value = list(0, 1, 2),
    support = list(reals, pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Variance - Alternate Scale Parameter"
    )
  )
  ps$addDeps("scale", "var", function(self) list(var = 2 * self$getParameterValue("scale")^2))
  ps$addDeps("var", "scale", function(self) list(scale = sqrt(self$getParameterValue("var") / 2)))

  return(ps)
}

getParameterSet.Logarithmic <- function(object, ...) {
  pset(
    prm("theta", Interval$new(0, 1, type = "()"), 0.5)
  )
}

getParameterSet.Logistic <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("mean", "scale", "sd"), value = list(0, 1, pi / sqrt(3)),
    support = list(reals, pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Standard Deviation - Alternative Scale Parameter"
    )
  )
  ps$addDeps("scale", "sd",
             function(self) list(sd = self$getParameterValue("scale") * pi / sqrt(3)))
  ps$addDeps("sd", "scale",
             function(self) list(scale = self$getParameterValue("sd") * sqrt(3) / pi))

  return(ps)
}

getParameterSet.Loglogistic <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("rate", "posreals", 1, "scales"),
    prm("scale", "posreals", tags = "scales"),
    tag_properties = list(linked = "scales"),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$rate <- 1 / x$scale
      } else if (!is.null(x$rate)) {
        x$scale <- 1 / x$rate
      }
      x
    }
  )
}

getParameterSet.Lognormal <- function(object, ...) {

    ps <- ParameterSet$new(
      id = list("meanlog", "varlog", "sdlog", "preclog", "mean", "var", "sd", "prec"),
      value = list(
        0, 1, 1, 1, exp(0.5), (exp(1) - 1) * exp(1), sqrt((exp(1) - 1) * exp(1)),
        ((exp(1) - 1) * exp(1))^-1
      ),
      support = list(
        reals, pos_reals, pos_reals, pos_reals, pos_reals,
        pos_reals, pos_reals, pos_reals
      ),
      description = list(
        "meanlog - Location Parameter on log scale",
        "varlog - Squared Scale Parameter on log scale",
        "sdlog - Scale Parameter on log scale",
        "preclog - Inverse Squared Scale Parameter on logscale",
        "mean - Location Parameter",
        "var - Squared Scale Parameter",
        "sd - Scale Parameter",
        "prec - Inverse Squared Scale Parameter"
      )
    )


    ps$addDeps("varlog", c("sdlog", "preclog", "mean", "var", "sd", "prec"),
               function(self) {
                 varlog <- self$getParameterValue("varlog")
                 meanlog <- self$getParameterValue("meanlog")
                 var <- (exp(varlog) - 1) * exp(2 * meanlog + varlog)
                 list(
                   sdlog = varlog^0.5,
                   preclog =  varlog^-1,
                   mean = exp(meanlog + varlog / 2),
                   var = var,
                   sd =  sqrt(var),
                   prec = 1 / var
                 )
               })

    ps$addDeps("sdlog", c("varlog", "preclog", "mean", "var", "sd", "prec"),
               function(self) {
                 sdlog <- self$getParameterValue("sdlog")
                 varlog <- sdlog^2
                 meanlog <- self$getParameterValue("meanlog")
                 var <- (exp(varlog) - 1) * exp(2 * meanlog + varlog)
                 list(
                   varlog = varlog,
                   preclog =  varlog^-1,
                   mean = exp(meanlog + varlog / 2),
                   var = var,
                   sd =  sqrt(var),
                   prec = 1 / var
                 )
               })
    ps$addDeps("preclog", c("varlog", "sdlog", "mean", "var", "sd", "prec"),
               function(self) {
                 preclog <- self$getParameterValue("preclog")
                 varlog <- 1 / preclog
                 meanlog <- self$getParameterValue("meanlog")
                 var <- (exp(varlog) - 1) * exp(2 * meanlog + varlog)
                 list(
                   varlog = varlog,
                   sdlog =  sqrt(varlog),
                   mean = exp(meanlog + varlog / 2),
                   var = var,
                   sd =  sqrt(var),
                   prec = 1 / var
                 )
               })
    ps$addDeps("var", c("meanlog", "varlog", "sdlog", "preclog", "sd", "prec"),
               function(self) {
                 var <- self$getParameterValue("var")
                 mean <- self$getParameterValue("mean")
                 varlog <- log(1 + var / mean^2)
                 list(
                   meanlog = log(mean / sqrt(1 + var / mean^2)),
                   varlog = varlog,
                   sdlog = sqrt(varlog),
                   preclog = 1 / varlog,
                   sd = sqrt(var),
                   prec = 1 / var
                 )
               })

    ps$addDeps("sd", c("var", "meanlog", "varlog", "sdlog", "preclog", "prec"),
               function(self) {
                 sd <- self$getParameterValue("sd")
                 var <- sd^2
                 mean <- self$getParameterValue("mean")
                 varlog <- log(1 + var / mean^2)
                 list(
                   var = var,
                   meanlog = log(mean / sqrt(1 + var / mean^2)),
                   varlog = varlog,
                   sdlog = sqrt(varlog),
                   preclog = 1 / varlog,
                   prec = 1 / var
                 )
               })

    ps$addDeps("prec", c("var", "meanlog", "varlog", "sdlog", "preclog", "sd"),
               function(self) {
                 prec <- self$getParameterValue("prec")
                 var <- 1 / prec
                 mean <- self$getParameterValue("mean")
                 varlog <- log(1 + var / mean^2)
                 list(
                   var = var,
                   meanlog = log(mean / sqrt(1 + var / mean^2)),
                   varlog = varlog,
                   sdlog = sqrt(varlog),
                   preclog = 1 / varlog,
                   sd = sqrt(var)
                 )
               })
    ps$addDeps("mean", "meanlog", function(self) {
      mean <- self$getParameterValue("mean")
      list(meanlog = log(mean / sqrt(1 + self$getParameterValue("var") / mean^2)))
    })
    ps$addDeps("meanlog", "mean", function(self) {
      list(mean = exp(self$getParameterValue("meanlog") +
            self$getParameterValue("varlog") / 2))
    })

  return(ps)
}

getParameterSet.Multinomial <- function(object, ...) {
  pset(
    prm("size", "posnaturals", 10),
    prm("probs", Interval$new(0, 1)^"n", rep(0.5, 2)),
    trafo = function(x, self) {
      if (!is.null(x$probs)) {
        x$probs <- x$probs / sum(x$probs)
      }
      x
    }
  )
}

getParameterSet.MultivariateNormal <- function(object, ...) { # nolint

  ps <- ParameterSet$new(
    id = list("mean", "cov", "prec"),
    value = list(
      rep(0, 2),
      matrix(c(1, 0, 0, 1), nrow = 2),
      matrix(c(1, 0, 0, 1), nrow = 2)
    ),
    support = list(
      setpower(reals, "n"),
      setpower(reals, "n"),
      setpower(reals, "n")
    ),
    description = list(
      "Vector of means - Location Parameter.",
      "Covariance matrix - Scale Parameter.",
      "Precision matrix - Scale Parameter."
    )
  )

  ps$addDeps("cov", "prec", function(self) {
    list(prec = list(solve(matrix(self$getParameterValue("cov"),
      nrow = length(self$getParameterValue("mean"))
    ))))
  })
  ps$addDeps("prec", "cov", function(self) {
   list(cov = list(solve(matrix(self$getParameterValue("prec"),
      nrow = length(self$getParameterValue("mean"))
    ))))
  })
  ps$addChecks(function(self) {
    mean <- self$getParameterValue("mean")
    if (checkmate::testList(mean)) {
      n <- length(mean[[1]])^2 * length(mean)
    } else {
      n <- length(mean)^2
    }
    n == length(unlist(self$getParameterValue("cov")))
    })

  return(ps)
}

getParameterSet.NegativeBinomial <- function(object, form = "fbs", ...) { # nolint

  if (form == "sbf") {
    desc <- "Number of failures"
  } else if (form == "tbf") {
    desc <- "Number of failures"
  } else if (form == "tbs") {
    desc <- "Number of successes"
  } else {
    desc <- "Number of successes"
  }

  ps <- ParameterSet$new(
    id = list("prob", "qprob", "mean", "size", "form"),
    value = list(0.5, 0.5, 10, 10, form),
    support = list(
      Interval$new(0, 1, type = "()"),
      Interval$new(0, 1, type = "()"),
      pos_reals,
      pos_naturals,
      Set$new("sbf", "tbf", "tbs", "fbs")
    ),
    settable = list(TRUE, TRUE, TRUE, TRUE, FALSE),
    description = list(
      "Probability of Success",
      "Probability of failure",
      "Mean - Location Parameter", desc, "Distribution form"
    )
  )

  if (form == "sbf") {
    ps$addDeps("size", "mean", function(self) {
      prob <- self$getParameterValue("prob")
      list(mean = self$getParameterValue("size") * prob / (1 - prob))
    })
    ps$addDeps("prob", c("qprob", "mean"), function(self) {
      prob <- self$getParameterValue("prob")
      list(
        qprob = 1 - prob,
        mean = self$getParameterValue("size") * prob / (1 - prob))
    })
    ps$addDeps("qprob", c("prob", "mean"), function(self) {
      qprob <- self$getParameterValue("qprob")
      list(
        prob = 1 - qprob,
        mean = self$getParameterValue("size") * (1 - qprob) / qprob)
    })
    ps$addDeps("mean", c("prob", "qprob"), function(self) {
      mean <- self$getParameterValue("mean")
      prob <- mean / (self$getParameterValue("size") + mean)
      list(
        prob = prob,
        qprob = 1 - prob
      )
    })
  } else if (form == "tbf") {
    ps$addDeps("size", "mean", function(self) {
      list(mean = self$getParameterValue("size") / (1 - self$getParameterValue("prob")))
    })
    ps$addDeps("prob", c("qprob", "mean"), function(self) {
      prob <- self$getParameterValue("prob")
      list(
        qprob = 1 - prob,
        mean = self$getParameterValue("size") / (1 - prob)
        )
    })
    ps$addDeps("qprob", c("prob", "mean"), function(self) {
      qprob <- self$getParameterValue("qprob")
      list(
        prob = 1 - qprob,
        mean = self$getParameterValue("size") / qprob)
    })
    ps$addDeps("mean", c("prob", "qprob"), function(self) {
      mean <- self$getParameterValue("mean")
      prob <- (mean - self$getParameterValue("size")) / mean
      list(
        prob = prob,
        qprob = 1 - prob
      )
    })
    ps$addChecks(function(self) all(unlist(self$getParameterValue("mean")) >=
                   unlist(self$getParameterValue("size"))))
  } else if (form == "tbs") {
    ps$addDeps("size", "mean", function(self) {
      list(mean = self$getParameterValue("size") /
        self$getParameterValue("prob"))
    })
    ps$addDeps("prob", c("qprob", "mean"), function(self) {
      prob <- self$getParameterValue("prob")
      list(
        qprob = 1 - prob,
        mean = self$getParameterValue("size") / prob
        )
    })
    ps$addDeps("qprob", c("prob", "mean"), function(self) {
      qprob <- self$getParameterValue("qprob")
      list(
        prob = 1 - qprob,
        mean = self$getParameterValue("size") / (1 - qprob))
    })
    ps$addDeps("mean", c("prob", "qprob"), function(self) {
      prob <- self$getParameterValue("size") / self$getParameterValue("mean")
      list(
        prob = prob,
        qprob = 1 - prob
      )
    })
    ps$addChecks(function(self) all(unlist(self$getParameterValue("mean")) >=
                                      unlist(self$getParameterValue("size"))))
  } else {
    ps$addDeps("size", "mean", function(self) {
      prob <- self$getParameterValue("prob")
      list(mean = self$getParameterValue("size") * (1 - prob) / prob)
    })
    ps$addDeps("prob", c("qprob", "mean"), function(self) {
      prob <- self$getParameterValue("prob")
      list(
        qprob = 1 - prob,
        mean = self$getParameterValue("size") * (1 - prob) / prob)
    })
    ps$addDeps("qprob", c("prob", "mean"), function(self) {
      qprob <- self$getParameterValue("qprob")
      list(
        prob = 1 - qprob,
        mean = self$getParameterValue("size") * qprob / (1 - qprob))
    })
    ps$addDeps("mean", c("prob", "qprob"), function(self) {
      size <- self$getParameterValue("size")
      prob <- size / (self$getParameterValue("mean") + size)
      list(
        prob = prob,
        qprob = 1 - prob
      )
    })
  }

  return(ps)
}

getParameterSet.Normal <- function(object, ...) {
  ps <- ParameterSet$new(
    id = list("mean", "var", "sd", "prec"),
    value = list(0, 1, 1, 1),
    support = list(reals, pos_reals, pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Variance - Squared Scale Parameter",
      "Standard Deviation - Scale Parameter",
      "Precision - Inverse Squared Scale Parameter"
    )
  )
  ps$addDeps("var", c("sd", "prec"), function(self) {
    var <- self$getParameterValue("var")
    list(
      sd = sqrt(var),
      prec = 1 / var
    )
  })
  ps$addDeps("sd", c("var", "prec"), function(self) {
    sd <- self$getParameterValue("sd")
    list(
      var = sd^2,
      prec = sd^-2
    )
  })
  ps$addDeps("prec", c("var", "sd"), function(self) {
    prec <- self$getParameterValue("prec")
    list(
      var = 1 / prec,
      sd = prec^-0.5
    )
  })

  return(ps)
}

getParameterSet.Pareto <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1),
    prm("scale", "posreals", 1)
  )
}

getParameterSet.Poisson <- function(object, ...) {
  pset(
    prm("rate", "posreals", 1)
  )
}

getParameterSet.Rayleigh <- function(object, ...) {
  pset(
    prm("mode", "posreals", 1)
  )
}

getParameterSet.ShiftedLoglogistic <- function(object, ...) { # nolint
  pset(
    prm("shape", "reals", 1),
    prm("location", "reals", 0),
    prm("scale", "posreals", tags = "scales"),
    prm("rate", "posreals", 1, "scales"),
    tag_properties = list(linked = "scales"),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$rate <- 1 / x$scale
      } else if (!is.null(x$rate)) {
        x$scale <- 1 / x$rate
      }
      x
    }
  )
}

getParameterSet.StudentT <- function(object, ...) {
  pset(
    prm("df", "posreals", 1)
  )
}

getParameterSet.StudentTNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df", "posreals", 1),
    prm("location", "reals", 0)
  )
}

getParameterSet.Triangular <- function(object, symmetric = FALSE, ...) {

  ps <- ParameterSet$new(
    id = list("lower", "upper", "mode", "symmetric"),
    value = list(0, 1, 0.5, symmetric),
    support = list(reals, reals, reals, Logicals$new()),
    settable = list(TRUE, TRUE, !symmetric, FALSE),
    description = list(
      "Lower distribution limit.", "Upper distribution limit.",
      "Distribution mode.", "Type of distribution."
    )
  )

  if (symmetric) {
    ps$addDeps("lower", "mode", function(self) {
      list(mode = (self$getParameterValue("lower") +
        self$getParameterValue("upper")) / 2)
    })
    ps$addDeps("upper", "mode", function(self) {
      list(mode = (self$getParameterValue("lower") +
        self$getParameterValue("upper")) / 2)
    })
  }

  ps$addChecks(function(self) all(unlist(self$getParameterValue("lower")) <
                 unlist(self$getParameterValue("upper"))))

  return(ps)
}

getParameterSet.Uniform <- function(object, ...) {
  pset(
    prm("lower", "reals", 0),
    prm("upper", "reals", 1),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("lt", id = "upper"))
    )
  )
}

getParameterSet.Wald <- function(object, ...) {
  pset(
    prm("mean", "posreals", 1),
    prm("shape", "posreals", 1)
  )
}

getParameterSet.Weibull <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("shape", "scale", "altscale"), value = list(1, 1, 1),
    support = list(pos_reals, pos_reals, pos_reals),
    description = list("Shape parameter", "Scale parameter", "Alternate scale parameter")
  )

  ps$addDeps("scale", "altscale", function(self) {
    list(altscale = self$getParameterValue("scale")^-self$getParameterValue("shape"))
  })
  ps$addDeps("altscale", "scale", function(self) {
    list(scale = exp(log(self$getParameterValue("altscale")) /
      (-self$getParameterValue("shape"))))
  })

  return(ps)
}

getParameterSet.WeightedDiscrete <- function(object, ...) { # nolint

  ps <- ParameterSet$new(
    id = list("x", "pdf", "cdf"),
    value = list(1, 1, 1),
    support = list(reals^"n", Interval$new(0, 1)^"n", Interval$new(0, 1)^"n"),
    description = list(
      "Data.", "Probability density function.",
      "Cumulative distribution function."
    )
  )
  ps$addDeps("pdf", "cdf", function(self) list(cdf = cumsum(self$getParameterValue("pdf"))))
  ps$addDeps("cdf", "pdf", function(self) {
    list(pdf = c(self$getParameterValue("cdf")[1], diff(self$getParameterValue("cdf"))))
  })
  ps$addChecks(function(self) {
    x <- unlist(self$getParameterValue("x"))
    all(length(unlist(self$getParameterValue("cdf"))) == length(x)) &&
      all(vapply(self$getParameterValue("x"), function(.x) !any(duplicated(.x)), logical(1)))
  })

  return(ps)
}
