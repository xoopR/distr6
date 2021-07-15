getParameterSet <- function(object, ...) {
  UseMethod("getParameterSet", object)
}

getParameterSet.Arcsine <- function(object, ...) {
  pset(
    prm("lower", "reals", 0, "required"),
    prm("upper", "reals", 1, "required"),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("leq", id = "upper"))
    )
  )
}

getParameterSet.Bernoulli <- function(object, ...) {
  pset(
    prm("prob", Interval$new(0, 1), 0.5, c("linked", "required")),
    prm("qprob", Interval$new(0, 1), tags = c("linked", "required")),
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
    prm("shape1", "posreals", 1, tags = "required"),
    prm("shape2", "posreals", 1, tags = "required")
  )
}

getParameterSet.BetaNoncentral <- function(object, ...) {
  pset(
    prm("shape1", "posreals", 1, tags = "required"),
    prm("shape2", "posreals", 1, tags = "required"),
    prm("location", "posreals0", 1, tags = "required")
  )
}

getParameterSet.Binomial <- function(object, ...) {
  pset(
    prm("prob", Interval$new(0, 1), 0.5, "probs", tags = c("linked", "required")),
    prm("qprob", Interval$new(0, 1), tags = "probs", tags = c("linked", "required")),
    prm("size", "posnaturals", 10, tags = "required"),
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
    prm("elements", "universal", 1, tags = "required"),
    prm("probs", Interval$new(0, 1)^"n", 1, tags = "required"),
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
    prm("location", "reals", 0, tags = "required"),
    prm("scale", "posreals", 1, tags = "required")
  )
}

getParameterSet.ChiSquared <- function(object, ...) {
  pset(
    prm("df", "posreals0", 1, tags = "required")
  )
}

getParameterSet.ChiSquaredNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df", "posreals0", 1, tags = "required"),
    prm("location", "posreals0", 0, tags = "required")
  )
}

getParameterSet.Degenerate <- function(object, ...) {
  pset(
    prm("mean", "reals", 0, tags = "required")
  )
}

getParameterSet.Dirichlet <- function(object, ...) {
  pset(
    prm("params", "nposreals", rep(1, 2), tags = "required")
  )
}

getParameterSet.DiscreteUniform <- function(object, ...) { # nolint
  pset(
    prm("lower", "integers", 0, tags = "required"),
    prm("upper", "integers", 1, tags = "required"),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("lt", id = "upper"))
    )
  )
}

getParameterSet.Erlang <- function(object, ...) {
  pset(
    prm("shape", "posintegers", 1, tags = "required"),
    prm("rate", "posreals", 1, , tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
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
    prm("rate", "posreals", 1, tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
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
    prm("df1", "posreals", 1, tags = "required"),
    prm("df2", "posreals", 1, tags = "required")
  )
}

getParameterSet.FDistributionNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df1", "posreals", 1, tags = "required"),
    prm("df2", "posreals", 1, tags = "required"),
    prm("location", "posreals0", 0, tags = "required")
  )
}

getParameterSet.Frechet <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("scale", "posreals", 1, tags = "required"),
    prm("minimum", "reals", 0, tags = "required")
  )
}

getParameterSet.Gamma <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("rate", "posreals", 1, , tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
    prm("mean", "posreals", tags = c("linked", "required")),
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
  type <- ifelse(trials, "()", "(]")

  ps <- pset(
    prm("prob", Interval$new(0, 1, type = type), 0.5, tags = c("linked", "required")),
    prm("qprob", Interval$new(0, 1, type = type), tags = c("linked", "required")),
    prm("trials", "logicals", trials, tags = "immutable")
  )

  ps$trafo <- function(x, self) {
    if (!is.null(x$qprob)) {
      x$prob <- 1 - x$qprob
    } else if (!is.null(x$prob)) {
      x$qprob <- 1 - x$prob
    }
    x
  }

  ps
}

getParameterSet.Gompertz <- function(object, ...) {
  pset(
    prm("shape", "posreals", tags = "required"),
    prm("scale", "posreals", tags = "required")
  )
}

getParameterSet.Gumbel <- function(object, ...) {
  pset(
    prm("location", "reals", 0, tags = "required"),
    prm("scale", "posreals", 1, tags = "required")
  )
}

getParameterSet.Hypergeometric <- function(object, ...) {
  pset(
    prm("size", "naturals", 50, tags = "required"),
    prm("successes", Set$new(0:50, class = "integer"), tags = c("linked", "required")),
    prm("failures", Set$new(0:50, class = "integer"), 45, tags = c("linked", "required")),
    prm("draws", Set$new(0:50, class = "integer"), 10, tags = "required"),
    trafo = function(x, self) {
      size <- ifelse(is.null(x$size), self$values$size, x$size)
      if (!is.null(x$successes)) {
        x$failures <- size - x$successes
      } else if (!is.null(x$failures)) {
        x$successes <- size - x$failures
      }
      x
    }
  )
}

getParameterSet.InverseGamma <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("scale", "posreals", 1, tags = "required")
  )
}

getParameterSet.Laplace <- function(object, ...) {
  pset(
    prm("mean", "reals", 0, tags = "required"),
    prm("scale", "posreals", 1, tags = c("linked", "required")),
    prm("var", "posreals", tags = c("linked", "required")),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$var <- 2 * x$scale^2
      } else if (!is.null(x$var)) {
        x$scale <- sqrt(x$var / 2)
      }
      x
    }
  )
}

getParameterSet.Logarithmic <- function(object, ...) {
  pset(
    prm("theta", Interval$new(0, 1, type = "()"), 0.5, tags = "required")
  )
}

getParameterSet.Logistic <- function(object, ...) {
  pset(
    prm("mean", "reals", 0, tags = "required"),
    prm("scale", "posreals", 1, tags = c("linked", "required")),
    prm("sd", "posreals", pi / sqrt(3), tags = c("linked", "required")),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$sd <- x$scale * pi / sqrt(3)
      } else if (!is.null(x$sd)) {
        x$scale <- x$sd * sqrt(3) / pi
      }
      x
    }
  )
}

getParameterSet.Loglogistic <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("rate", "posreals", 1, tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
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
    prm("size", "posnaturals", 10, tags = "required"),
    prm("probs", Interval$new(0, 1)^"n", rep(0.5, 2), tags = "required"),
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
  pset(
    prm("mean", "reals", 0, tags = "required"),
    prm("var", "posreals", 1, tags = c("linked", "required")),
    prm("sd", "posreals", tags = c("linked", "required")),
    prm("prec", "posreals", tags = c("linked", "required")),
    trafo = function(x, self) {
      if (!is.null(x$var)) {
        x$sd <- sqrt(x$var)
        x$prec <- 1 / x$var
      } else if (!is.null(x$sd)) {
        x$var <- x$sd^2
        x$prec <- x$sd^-2
      } else if (!is.null(x$prec)) {
        x$var <- 1 / x$prec
        x$sd <- x$prec^-0.5
      }
      x
    }
  )
}

getParameterSet.Pareto <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("scale", "posreals", 1, tags = "required")
  )
}

getParameterSet.Poisson <- function(object, ...) {
  pset(
    prm("rate", "posreals", 1, tags = "required")
  )
}

getParameterSet.Rayleigh <- function(object, ...) {
  pset(
    prm("mode", "posreals", 1, tags = "required")
  )
}

getParameterSet.ShiftedLoglogistic <- function(object, ...) { # nolint
  pset(
    prm("shape", "reals", 1, tags = "required"),
    prm("location", "reals", 0, tags = "required"),
    prm("scale", "posreals", tags = c("linked", "required")),
    prm("rate", "posreals", 1, tags = c("linked", "required")),
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
    prm("df", "posreals", 1, tags = "required")
  )
}

getParameterSet.StudentTNoncentral <- function(object, ...) { # nolint
  pset(
    prm("df", "posreals", 1, tags = "required"),
    prm("location", "reals", 0, tags = "required")
  )
}

getParameterSet.Triangular <- function(object, symmetric = FALSE, ...) {
  mode_tag <- if (symmetric) "immutable" else "required"
  mode_val <- if (symmetric) NULL else 0.5

  ps <- pset(
    prm("lower", "reals", 0, tags = "required"),
    prm("upper", "reals", 1, tags = "required"),
    prm("mode", "reals", mode_val, tags = mode_tag),
    prm("symmetric", "logicals", symmetric, tags = "immutable"),
    deps = list(
      list(id = "mode", on = "lower", cnd = cnd("gt", id = "lower")),
      list(id = "mode", on = "upper", cnd = cnd("lt", id = "upper"))
    )
  )

  if (symmetric) {
    ps$trafo <- function(x, self) {
      lower <- ifelse(!is.null(x$lower), x$lower, self$values$lower)
      upper <- ifelse(!is.null(x$upper), x$upper, self$values$upper)
      x$mode <- (lower + upper) / 2
      x
    }
  }

  ps
}

getParameterSet.Uniform <- function(object, ...) {
  pset(
    prm("lower", "reals", 0, tags = "required"),
    prm("upper", "reals", 1, tags = "required"),
    deps = list(
      list(id = "lower", on = "upper", cnd = cnd("lt", id = "upper"))
    )
  )
}

getParameterSet.Wald <- function(object, ...) {
  pset(
    prm("mean", "posreals", 1, tags = "required"),
    prm("shape", "posreals", 1, tags = "required")
  )
}

getParameterSet.Weibull <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("scale", "posreals", 1, tags = c("linked", "required")),
    prm("altscale", "posreals", tags = c("linked", "required")),
    trafo = function(x, self) {
      if (!is.null(x$scale)) {
        x$altscale <- x$scale^-x$shape
      } else if (!is.null(x$altscale)) {
        x$scale <- exp(log(x$altscale) / -x$shape)
      }
      x
    }
  )
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
