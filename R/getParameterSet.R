getParameterSet <- function(object, ...) {
  UseMethod("getParameterSet", object)
}

getParameterSet.Arcsine <- function(object, ...) {
  ps <- ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(reals, reals),
    description = list(
      "Lower distribution limit.",
      "Upper distribution limit."
    )
  )
  ps$addChecks(function(self) all(unlist(self$getParameterValue("lower")) <=
                                    unlist(self$getParameterValue("upper"))))
  return(ps)
}

getParameterSet.Bernoulli <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("prob", "qprob"), value = list(0.5, 0.5),
    support = list(Interval$new(0, 1), Interval$new(0, 1)),
    description = list("Probability of Success", "Probability of failure")
  )
  ps$addDeps("prob", "qprob", function(self) list(qprob = 1 - self$getParameterValue("prob")))
  ps$addDeps("qprob", "prob", function(self) list(prob = 1 - self$getParameterValue("qprob")))

  return(ps)
}

getParameterSet.Beta <- function(object, ...) {

  ParameterSet$new(
    id = list("shape1", "shape2"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape Parameter (alpha)", "Shape Parameter (beta)")
  )
}

getParameterSet.BetaNoncentral <- function(object, ...) {
  ParameterSet$new(
    id = list("shape1", "shape2", "location"), value = list(1, 1, 0),
    support = list(pos_reals, pos_reals, PosReals$new(zero = TRUE)),
    description = list(
      "Shape Parameter (alpha)", "Shape Parameter (beta)",
      "Non-centrality parameter"
    )
  )
}

getParameterSet.Binomial <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("prob", "qprob", "size"), value = list(0.5, 0.5, 10),
    support = list(Interval$new(0, 1), Interval$new(0, 1), pos_naturals),
    description = list(
      "Probability of Success",
      "Probability of failure", "Number of trials"
    )
  )
  ps$addDeps("prob", "qprob", function(self) list(qprob = 1 - self$getParameterValue("prob")))
  ps$addDeps("qprob", "prob", function(self) list(prob = 1 - self$getParameterValue("qprob")))

  return(ps)
}

getParameterSet.Categorical <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("elements", "probs"),
    value = list(1, 1),
    support = list(
      Universal$new(), setpower(Interval$new(0, 1), "n")
    ),
    settable = list(TRUE, TRUE),
    description = list("Categories", "Probability of success i")
  )

  ps$addChecks(function(self) all(length(unlist(self$getParameterValue("probs"))) ==
                 length(unlist(self$getParameterValue("elements")))))
  ps$addTrafos("probs", function(x, self) x / sum(x))

  return(ps)
}

getParameterSet.Cauchy <- function(object, ...) {
  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(reals, pos_reals),
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
  )
}

getParameterSet.ChiSquared <- function(object, ...) {
  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(PosReals$new(zero = TRUE)),
    description = list("Degrees of Freedom")
  )
}

getParameterSet.ChiSquaredNoncentral <- function(object, ...) { # nolint
  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(PosReals$new(zero = TRUE), PosReals$new(zero = TRUE)),
    description = list("Degrees of Freedom", "Non-centrality parameter")
  )
}

getParameterSet.Degenerate <- function(object, ...) {
  ParameterSet$new(
    id = list("mean"), value = list(0),
    support = list(reals),
    description = list("Location Parameter")
  )
}

getParameterSet.Dirichlet <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("params"),
    value = list(rep(1, 2)),
    support = list(
      setpower(pos_reals, "n")
    ),
    settable = list(TRUE),
    description = list("Concentration parameters")
  )

  return(ps)
}

getParameterSet.DiscreteUniform <- function(object, ...) { # nolint

  ps <- ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(Integers$new(), Integers$new()),
    settable = list(TRUE, TRUE),
    description = list(
      "Lower distribution limit.", "Upper distribution limit."
    )
  )

  ps$addChecks(function(self) all(unlist(self$getParameterValue("lower")) <
                 unlist(self$getParameterValue("upper"))))

  return(ps)
}

getParameterSet.Erlang <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("shape", "rate", "scale"), value = list(1, 1, 1),
    support = list(PosIntegers$new(), pos_reals, pos_reals),
    description = list(
      "Shape - Shape Parameter",
      "Rate - Inverse Scale Parameter",
      "Scale - Scale Parameter"
    )
  )
  ps$addDeps("rate", "scale", function(self) list(scale = self$getParameterValue("rate")^-1))
  ps$addDeps("scale", "rate", function(self) list(rate = self$getParameterValue("scale")^-1))

  return(ps)
}

getParameterSet.Exponential <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("rate", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Arrival Rate", "Scale")
  )
  ps$addDeps("rate", "scale", function(self) list(scale = self$getParameterValue("rate")^-1))
  ps$addDeps("scale", "rate", function(self) list(rate = self$getParameterValue("scale")^-1))

  return(ps)

}

getParameterSet.FDistribution <- function(object, ...) {

  ParameterSet$new(
    id = list("df1", "df2"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list(
      "Degrees of freedom 1",
      "Degrees of freedom 2"
    )
  )
}

getParameterSet.FDistributionNoncentral <- function(object, ...) { # nolint

  ParameterSet$new(
    id = list("df1", "df2", "location"), value = list(1, 1, 0),
    support = list(pos_reals, pos_reals, PosReals$new(zero = TRUE)),
    description = list(
      "Degrees of freedom 1",
      "Degrees of freedom 2",
      "Non-centrality parameter"
    )
  )
}

getParameterSet.Frechet <- function(object, ...) {

  ParameterSet$new(
    id = list("shape", "scale", "minimum"), value = list(1, 1, 0),
    support = list(pos_reals, pos_reals, reals),
    description = list(
      "Shape Parameter", "Scale Parameter",
      "Distribution Minimum - Location Parameter"
    )
  )

}

getParameterSet.Gamma <- function(object, ...) {

  ps <- ParameterSet$new(
    id = list("shape", "rate", "scale", "mean"), value = list(1, 1, 1, 1),
    support = list(pos_reals, pos_reals, pos_reals, pos_reals),
    description = list(
      "Shape - Shape Parameter",
      "Rate - Inverse Scale Parameter",
      "Scale - Scale Parameter",
      "Mean - Mean Parameter"
    )
  )
  ps$addDeps("rate", c("scale", "mean"), function(self) {
    rate <- self$getParameterValue("rate")
    list(scale = 1 / rate,
         mean = self$getParameterValue("shape") / rate)
  })
  ps$addDeps("scale", c("rate", "mean"), function(self) {
    scale <- self$getParameterValue("scale")
    list(rate = 1 / scale,
         mean = self$getParameterValue("shape") * scale)
  })
  ps$addDeps("mean", c("rate", "scale"), function(self) {
    rate <- self$getParameterValue("shape") / self$getParameterValue("mean")
    list(rate, scale = rate^-1)
  })

  return(ps)
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
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Gumbel <- function(object, ...) {
  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(reals, pos_reals),
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
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
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape Parameter", "Scale Parameter")
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
  ParameterSet$new(
    id = list("theta"), value = list(0.5),
    support = list(Interval$new(0, 1, type = "()")),
    description = list("Theta parameter.")
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

  ps <- ParameterSet$new(
    id = list("scale", "rate", "shape"), value = list(1, 1, 1),
    support = list(pos_reals, pos_reals, pos_reals),
    description = list(
      "Scale Parameter",
      "Rate Parameter",
      "Shape Parameter"
    )
  )

  ps$addDeps("rate", "scale", function(self) list(scale = self$getParameterValue("rate")^-1))
  ps$addDeps("scale", "rate", function(self) list(rate = self$getParameterValue("scale")^-1))

  return(ps)
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

  ps <- ParameterSet$new(
    id = list("size", "probs"),
    value = list(10, rep(0.5, 2)),
    support = list(pos_naturals, setpower(Interval$new(0, 1), "n")),
    settable = list(TRUE, TRUE),
    description = list(
      "Number of trials", "Probability of success i"
    )
  )

  ps$addTrafos("probs", function(x, self) x / sum(x))

  return(ps)
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
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Poisson <- function(object, ...) {
  ParameterSet$new(
    id = list("rate"), value = list(1),
    support = list(pos_reals),
    description = list("Arrival Rate")
  )
}

getParameterSet.Rayleigh <- function(object, ...) {
  ParameterSet$new(
    id = list("mode"), value = list(1),
    support = list(pos_reals),
    description = list("Mode - Scale Parameter")
  )
}

getParameterSet.ShiftedLoglogistic <- function(object, ...) { # nolint

  ps <- ParameterSet$new(
    id = list("scale", "rate", "shape", "location"), value = list(1, 1, 1, 0),
    support = list(pos_reals, pos_reals, reals, reals),
    description = list(
      "Scale Parameter",
      "Rate Parameter",
      "Shape Parameter",
      "Location Parameter"
    )
  )

  ps$addDeps("rate", "scale", function(self) list(scale = self$getParameterValue("rate")^-1))
  ps$addDeps("scale", "rate", function(self) list(rate = self$getParameterValue("scale")^-1))

  return(ps)
}

getParameterSet.StudentT <- function(object, ...) {
  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(pos_reals),
    description = list("Degrees of Freedom")
  )
}

getParameterSet.StudentTNoncentral <- function(object, ...) { # nolint

  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(pos_reals, reals),
    description = list("Degrees of Freedom", "Non-centrality parameter")
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
  ps <- ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(reals, reals),
    description = list("Lower distribution limit.", "Upper distribution limit.")
  )

  ps$addChecks(function(self) all(unlist(self$getParameterValue("lower")) <
                                    unlist(self$getParameterValue("upper"))))

  return(ps)
}

getParameterSet.Wald <- function(object, ...) {

  ParameterSet$new(
    id = list("mean", "shape"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Shape Parameter"
    )
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
    x = unlist(self$getParameterValue("x"))
    all(length(unlist(self$getParameterValue("cdf"))) == length(x)) &&
      all(vapply(self$getParameterValue("x"), function(.x) !any(duplicated(.x)), logical(1)))
  })

  return(ps)
}
