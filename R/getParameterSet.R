getParameterSet <- function(object, ...) {
  UseMethod("getParameterSet", object)
}

getParameterSet.Arcsine <- function(object, lower, upper) {
  ps <- ParameterSet$new(
    id = list("lower", "upper"), value = list(0, 1),
    support = list(reals, reals),
    description = list(
      "Lower distribution limit.",
      "Upper distribution limit."
    )
  )
  ps$addChecks("lower", function(x, self) x <= self$getParameterValue("upper"))
  ps$addChecks("upper", function(x, self) x >= self$getParameterValue("lower"))
  return(ps)
}

getParameterSet.Bernoulli <- function(object, prob, qprob = NULL) {

  # prob.bool <- qprob.bool <- FALSE
  #
  # if (!is.null(qprob)) {
  #   qprob.bool <- TRUE
  # } else if (!is.null(prob)) {
  #   prob.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("prob", "qprob"), value = list(0.5, 0.5),
    support = list(Interval$new(0, 1), Interval$new(0, 1)),
    description = list("Probability of Success", "Probability of failure")
  )
  ps$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
  ps$addDeps("qprob", "prob", function(self) 1 - self$getParameterValue("qprob"))

  return(ps)
}

getParameterSet.Beta <- function(object, shape1, shape2) {

  ParameterSet$new(
    id = list("shape1", "shape2"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape Parameter (alpha)", "Shape Parameter (beta)")
  )
}

getParameterSet.BetaNoncentral <- function(object, shape1, shape2, location) {
  ParameterSet$new(
    id = list("shape1", "shape2", "location"), value = list(1, 1, 0),
    support = list(pos_reals, pos_reals, PosReals$new(zero = TRUE)),
    description = list(
      "Shape Parameter (alpha)", "Shape Parameter (beta)",
      "Non-centrality parameter"
    )
  )
}

getParameterSet.Binomial <- function(object, size, prob, qprob = NULL) {

  # prob.bool <- qprob.bool <- FALSE
  #
  # if (!is.null(qprob)) {
  #   qprob.bool <- TRUE
  # } else {
  #   prob.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("prob", "qprob", "size"), value = list(0.5, 0.5, 10),
    support = list(Interval$new(0, 1), Interval$new(0, 1), pos_naturals),
    description = list(
      "Probability of Success",
      "Probability of failure", "Number of trials"
    )
  )
  ps$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
  ps$addDeps("qprob", "prob", function(self) 1 - self$getParameterValue("qprob"))

  return(ps)
}

getParameterSet.Categorical <- function(object, probs, elements) {

  nCategories <- length(probs)

  ps <- ParameterSet$new(
    id = list("elements", "probs"),
    value = list(rep(1, nCategories), rep(0.5, nCategories)),
    support = list(
      UniversalSet$new(), setpower(Interval$new(0, 1), "n")
    ),
    settable = list(TRUE, TRUE),
    description = list("Categories", "Probability of success i")
  )

  ps$addChecks("probs", function(x, self) length(x) == length(self$getParameterValue("elements")))
  ps$addChecks("elements", function(x, self) length(x) == length(self$getParameterValue("probs")))
  ps$addTrafos("probs", function(x, self) x / sum(x))

  return(ps)
}

getParameterSet.Cauchy <- function(object, location, scale) {
  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(reals, pos_reals),
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
  )
}

getParameterSet.ChiSquared <- function(object, df) {
  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(PosReals$new(zero = TRUE)),
    description = list("Degrees of Freedom")
  )
}

getParameterSet.ChiSquaredNoncentral <- function(object, df, location) { # nolint
  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(PosReals$new(zero = TRUE), PosReals$new(zero = TRUE)),
    description = list("Degrees of Freedom", "Non-centrality parameter")
  )
}

getParameterSet.Degenerate <- function(object, mean) {
  ParameterSet$new(
    id = list("mean"), value = list(0),
    support = list(reals),
    description = list("Location Parameter")
  )
}

getParameterSet.Dirichlet <- function(object, params) {

  K <- length(params)

  ps <- ParameterSet$new(
    id = list("params"),
    value = list(rep(1, K)),
    support = list(
      setpower(pos_reals, "n")
    ),
    settable = list(TRUE),
    description = list("Concentration parameters")
  )

  return(ps)
}

getParameterSet.DiscreteUniform <- function(object, lower, upper) { # nolint

  ps <- ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(Integers$new(), Integers$new()),
    settable = list(TRUE, TRUE),
    description = list(
      "Lower distribution limit.", "Upper distribution limit."
    )
  )

  ps$addChecks("lower", function(x, self) x < self$getParameterValue("upper"))
  ps$addChecks("upper", function(x, self) x > self$getParameterValue("lower"))

  return(ps)
}

getParameterSet.Erlang <- function(object, shape, rate, scale = NULL) {

  # rate.bool <- scale.bool <- FALSE
  #
  # if (!is.null(scale)) {
  #   scale.bool <- TRUE
  # } else {
  #   rate.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("shape", "rate", "scale"), value = list(1, 1, 1),
    support = list(PosIntegers$new(), pos_reals, pos_reals),
    description = list(
      "Shape - Shape Parameter",
      "Rate - Inverse Scale Parameter",
      "Scale - Scale Parameter"
    )
  )
  ps$addDeps("rate", "scale", function(self) self$getParameterValue("rate")^-1)
  ps$addDeps("scale", "rate", function(self) self$getParameterValue("scale")^-1)

  return(ps)
}

getParameterSet.Exponential <- function(object, rate, scale = NULL) {

  # rate.bool <- scale.bool <- FALSE
  #
  # if (!is.null(scale)) {
  #   scale.bool <- TRUE
  # } else {
  #   rate.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("rate", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Arrival Rate", "Scale")
  )
  ps$addDeps("rate", "scale", function(self) self$getParameterValue("rate")^-1)
  ps$addDeps("scale", "rate", function(self) self$getParameterValue("scale")^-1)

  return(ps)

}

getParameterSet.FDistribution <- function(object, df1, df2) {

  ParameterSet$new(
    id = list("df1", "df2"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list(
      "Degrees of freedom 1",
      "Degrees of freedom 2"
    )
  )
}

getParameterSet.FDistributionNoncentral <- function(object, df1, df2, location) { # nolint

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

getParameterSet.Frechet <- function(object, shape, scale, minimum) {

  ParameterSet$new(
    id = list("shape", "scale", "minimum"), value = list(1, 1, 0),
    support = list(pos_reals, pos_reals, reals),
    description = list(
      "Shape Parameter", "Scale Parameter",
      "Distribution Minimum - Location Parameter"
    )
  )

}

getParameterSet.Gamma <- function(object, shape, rate, scale = NULL, mean = NULL) {

  # rate.bool <- mean.bool <- scale.bool <- FALSE
  #
  # if (!is.null(mean)) {
  #   mean.bool <- TRUE
  # } else if (!is.null(scale)) {
  #   scale.bool <- TRUE
  # } else {
  #   rate.bool <- TRUE
  # }

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
  ps$addDeps("rate", "scale", function(self) 1 / self$getParameterValue("rate"))
  ps$addDeps(
    "rate", "mean",
    function(self) self$getParameterValue("shape") / self$getParameterValue("rate")
  )
  ps$addDeps("scale", "rate", function(self) 1 / self$getParameterValue("scale"))
  ps$addDeps(
    "scale", "mean",
    function(self) self$getParameterValue("shape") * self$getParameterValue("scale")
  )
  ps$addDeps(
    "mean", "rate",
    function(self) self$getParameterValue("shape") / self$getParameterValue("mean")
  )
  ps$addDeps(
    "mean", "scale",
    function(self) self$getParameterValue("mean") / self$getParameterValue("shape")
  )

  return(ps)
}

getParameterSet.Geometric <- function(object, prob, qprob = NULL, trials = FALSE) {

  # prob.bool <- qprob.bool <- FALSE
  #
  # if (!is.null(qprob)) {
  #   qprob.bool <- TRUE
  # } else {
  #   prob.bool <- TRUE
  # }

  if (trials) {
    ps <- ParameterSet$new(
      id = list("prob", "qprob", "trials"),
      value = list(0.5, 0.5, TRUE),
      support = list(
        Interval$new(0, 1, type = "()"),
        Interval$new(0, 1, type = "()"),
        LogicalSet$new()
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
        LogicalSet$new()
      ),
      settable = list(TRUE, TRUE, FALSE),
      description = list(
        "Probability of success",
        "Probability of failure",
        "Form: number of failures before first success"
      )
    )
  }

  ps$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
  ps$addDeps("qprob", "prob", function(self) 1 - self$getParameterValue("qprob"))

  return(ps)
}

getParameterSet.Gompertz <- function(object, shape, scale) {
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Gumbel <- function(object, location, scale) {
  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(reals, pos_reals),
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
  )
}

getParameterSet.Hypergeometric <- function(object, size, successes, failures = NULL, draws) {

  # successes.bool <- failures.bool <- FALSE
  #
  # if (!is.null(failures)) {
  #   failures.bool <- TRUE
  # } else if (!is.null(successes)) {
  #   successes.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("size", "successes", "failures", "draws"),
    value = list(1e08, 1e08, 0, 1e08),
    support = list(naturals, naturals, naturals, naturals),
    description = list(
      "Population size",
      "Number of successes in the population.",
      "Number of failures in the population.",
      "Number of draws."
    )
  )

  ps$addDeps(
    "successes", "failures",
    function(self) self$getParameterValue("size") - self$getParameterValue("successes")
  )
  ps$addDeps(
    "failures", "successes",
    function(self) self$getParameterValue("size") - self$getParameterValue("failures")
  )

  return(ps)

}

getParameterSet.InverseGamma <- function(object, shape, scale) {
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape Parameter", "Scale Parameter")
  )
}

getParameterSet.Laplace <- function(object, mean, scale, var = NULL) {

  # var.bool <- scale.bool <- FALSE
  #
  # if (!is.null(var)) {
  #   var.bool <- TRUE
  # } else {
  #   scale.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("mean", "scale", "var"), value = list(0, 1, 2),
    support = list(reals, pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Variance - Alternate Scale Parameter"
    )
  )
  ps$addDeps("scale", "var", function(self) 2 * self$getParameterValue("scale")^2)
  ps$addDeps("var", "scale", function(self) sqrt(self$getParameterValue("var") / 2))

  return(ps)
}

getParameterSet.Logarithmic <- function(object, theta) {
  ParameterSet$new(
    id = list("theta"), value = list(0.5),
    support = list(Interval$new(0, 1, type = "()")),
    description = list("Theta parameter.")
  )
}

getParameterSet.Logistic <- function(object, mean, scale, sd = NULL) {

  # sd.bool <- scale.bool <- FALSE
  #
  # if (!is.null(sd)) {
  #   sd.bool <- TRUE
  # } else {
  #   scale.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("mean", "scale", "sd"), value = list(0, 1, pi / sqrt(3)),
    support = list(reals, pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Standard Deviation - Alternative Scale Parameter"
    )
  )
  ps$addDeps("scale", "sd", function(self) self$getParameterValue("scale") * pi / sqrt(3))
  ps$addDeps("sd", "scale", function(self) self$getParameterValue("sd") * sqrt(3) / pi)

  return(ps)
}

getParameterSet.Loglogistic <- function(object, scale, shape, rate = NULL) {

  # rate.bool <- scale.bool <- FALSE
  #
  # if (!is.null(rate)) {
  #   rate.bool <- TRUE
  # } else {
  #   scale.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("scale", "rate", "shape"), value = list(1, 1, 1),
    support = list(pos_reals, pos_reals, pos_reals),
    description = list(
      "Scale Parameter",
      "Rate Parameter",
      "Shape Parameter"
    )
  )

  ps$addDeps("rate", "scale", function(self) self$getParameterValue("rate")^-1)
  ps$addDeps("scale", "rate", function(self) self$getParameterValue("scale")^-1)

  return(ps)
}

getParameterSet.Lognormal <- function(object, meanlog = NULL, varlog = NULL,
                                      sdlog = NULL, preclog = NULL,
                                      mean = NULL, var = NULL, sd = NULL, prec = NULL) {

  # varlog.bool <- sdlog.bool <- preclog.bool <- var.bool <- sd.bool <- prec.bool <- FALSE
  if (is.null(meanlog) & is.null(mean)) {
    stop("One of 'meanlog' and 'mean' should be provided.")
  }

  if (!is.null(meanlog)) {
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

    ps$addDeps("meanlog", "mean", function(self) {
      exp(self$getParameterValue("meanlog") +
        self$getParameterValue("varlog") / 2)
    })
    ps$addDeps("varlog", "sdlog", function(self) self$getParameterValue("varlog")^0.5)
    ps$addDeps("varlog", "preclog", function(self) self$getParameterValue("varlog")^-1)
    ps$addDeps("varlog", "mean", function(self) {
      exp(self$getParameterValue("meanlog") + self$getParameterValue("varlog") / 2)
    })
    ps$addDeps("varlog", "var", function(self) {
      (exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog"))
    })
    ps$addDeps("varlog", "sd", function(self) {
      sqrt((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))
    })
    ps$addDeps("varlog", "prec", function(self) {
      ((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))^(-1) # nolint
    })

    ps$addDeps("sdlog", "varlog", function(self) self$getParameterValue("sdlog")^2) # nolint
    ps$addDeps("sdlog", "preclog", function(self) self$getParameterValue("varlog")^-1) # nolint
    ps$addDeps("sdlog", "mean", function(self) {
      exp(self$getParameterValue("meanlog") + self$getParameterValue("varlog") / 2)
    })
    ps$addDeps("sdlog", "var", function(self) {
      (exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog"))
    })
    ps$addDeps("sdlog", "sd", function(self) {
      sqrt((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))
    })
    ps$addDeps("sdlog", "prec", function(self) {
      ((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))^(-1) # nolint
    })

    ps$addDeps("preclog", "varlog", function(self) self$getParameterValue("preclog")^-1) # nolint
    ps$addDeps("preclog", "sdlog", function(self) self$getParameterValue("varlog")^0.5) # nolint
    ps$addDeps("preclog", "mean", function(self) {
      exp(self$getParameterValue("meanlog") + self$getParameterValue("varlog") / 2)
    })
    ps$addDeps("preclog", "var", function(self) {
      (exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog"))
    })
    ps$addDeps("preclog", "sd", function(self) {
      sqrt((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))
    })
    ps$addDeps("preclog", "prec", function(self) {
      ((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") +
        self$getParameterValue("varlog")))^(-1) # nolint
    })

  } else {
    # if (!is.null(prec)) {
    #   prec.bool <- TRUE
    # } else if (!is.null(sd)) {
    #   sd.bool <- TRUE
    # } else if (!is.null(var)) {
    #   var.bool <- TRUE
    # }

    ps <- ParameterSet$new(
      id = list("meanlog", "varlog", "sdlog", "preclog", "mean", "var", "sd", "prec"),
      value = list(log(1 / sqrt(2)), log(2), sqrt(log(2)), 1 / log(2), 1, 1, 1, 1),
      support = list(
        reals, pos_reals, pos_reals, pos_reals, pos_reals,
        pos_reals, pos_reals, pos_reals
      ),
      description = list(
        "meanlog - Location Parameter on log scale",
        "varlog - Squared Scale Parameter on log scale",
        "sdlog - Scale Parameter on log scale",
        "preclog - Inverse Squared Scale Parameter on logscale",
        "meanlog - Location Parameter",
        "varlog - Squared Scale Parameter",
        "sdlog - Scale Parameter",
        "preclog - Inverse Squared Scale Parameter"
      )
    )

    ps$addDeps("mean", "meanlog", function(self) {
      log(self$getParameterValue("mean") / sqrt(1 +
        self$getParameterValue("var") /
          self$getParameterValue("mean")^2))
    })

    ps$addDeps("var", "meanlog", function(self) {
      log(self$getParameterValue("mean") / sqrt(1 +
        self$getParameterValue("var") /
          self$getParameterValue("mean")^2))
    })
    ps$addDeps("var", "varlog", function(self) {
      log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2)
    })
    ps$addDeps("var", "sdlog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^0.5
    })
    ps$addDeps("var", "preclog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^-1
    })
    ps$addDeps("var", "sd", function(self) self$getParameterValue("var")^0.5)
    ps$addDeps("var", "prec", function(self) self$getParameterValue("var")^-1)

    ps$addDeps("sd", "var", function(self) self$getParameterValue("var")^2)
    ps$addDeps("sd", "meanlog", function(self) {
      log(self$getParameterValue("mean") / sqrt(1 +
        self$getParameterValue("var") /
          self$getParameterValue("mean")^2))
    })
    ps$addDeps("sd", "varlog", function(self) {
      log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2)
    })
    ps$addDeps("sd", "sdlog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^0.5
    })
    ps$addDeps("sd", "preclog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^-1
    })
    ps$addDeps("sd", "prec", function(self) self$getParameterValue("var")^-1)

    ps$addDeps("prec", "var", function(self) self$getParameterValue("prec")^-1)
    ps$addDeps("prec", "meanlog", function(self) {
      log(self$getParameterValue("mean") / sqrt(1 +
        self$getParameterValue("var") /
          self$getParameterValue("mean")^2))
    })
    ps$addDeps("prec", "varlog", function(self) {
      log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2)
    })
    ps$addDeps("prec", "sdlog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^0.5
    })
    ps$addDeps("prec", "preclog", function(self) {
      (log(1 + self$getParameterValue("var") /
        self$getParameterValue("mean")^2))^-1
    })
    ps$addDeps("prec", "sd", function(self) self$getParameterValue("var")^0.5)
  }

  return(ps)
}

getParameterSet.Multinomial <- function(object, size, probs) {

  K <- unlist(length(probs))
  ps <- ParameterSet$new(
    id = list("size", "probs"),
    value = list(1, rep(0.5, K)),
    support = list(pos_naturals, setpower(Interval$new(0, 1), "n")),
    settable = list(TRUE, TRUE),
    description = list(
      "Number of trials", "Probability of success i"
    )
  )

  ps$addTrafos("probs", function(x, self) x / sum(x))

  return(ps)
}

getParameterSet.MultivariateNormal <- function(object, mean, cov, prec = NULL) { # nolint

  # cov.bool <- prec.bool <- FALSE
  #
  # if (!is.null(prec)) {
  #   prec.bool <- TRUE
  # } else {
  #   cov.bool <- TRUE
  # }

  K <- length(mean)

  ps <- ParameterSet$new(
    id = list("mean", "cov", "prec"),
    value = list(
      rep(0, K),
      matrix(rep(0, K^2), nrow = K),
      matrix(rep(0, K^2), nrow = K)
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
    list(solve(matrix(self$getParameterValue("cov"),
      nrow = length(self$getParameterValue("mean"))
    )))
  })
  ps$addDeps("prec", "cov", function(self) {
    list(solve(matrix(self$getParameterValue("prec"),
      nrow = length(self$getParameterValue("mean"))
    )))
  })
  ps$addChecks("mean", function(x, self)
    length(unlist(x)) == sqrt(length(unlist(self$getParameterValue("cov"))))
    )

  return(ps)
}

getParameterSet.NegativeBinomial <- function(object, size, prob, qprob = NULL, mean = NULL, form = "fbs") { # nolint

  # prob.bool <- qprob.bool <- mean.bool <- FALSE
  #
  # if (!is.null(mean)) {
  #   mean.bool <- TRUE
  # } else if (!is.null(qprob)) {
  #   qprob.bool <- TRUE
  # } else {
  #   prob.bool <- TRUE
  # }

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
    value = list(0.5, 0.5, 1, 20, form),
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

  ps$addDeps("prob", "qprob", function(self) 1 - self$getParameterValue("prob"))
  ps$addDeps("qprob", "prob", function(self) 1 - self$getParameterValue("qprob"))
  if (form == "sbf") {
    ps$addDeps("size", "mean", function(self) {
      self$getParameterValue("size") *
        self$getParameterValue("prob") / (1 - self$getParameterValue("prob"))
    })
    ps$addDeps("prob", "mean", function(self) {
      self$getParameterValue("size") *
        self$getParameterValue("prob") / (1 - self$getParameterValue("prob"))
    })
    ps$addDeps("qprob", "mean", function(self) {
      self$getParameterValue("size") *
        (1 - self$getParameterValue("qprob")) / self$getParameterValue("qprob")
    })
    ps$addDeps("mean", "prob", function(self) {
      self$getParameterValue("mean") /
        (self$getParameterValue("size") + self$getParameterValue("mean"))
    })
    ps$addDeps("mean", "qprob", function(self) {
      1 -
        (self$getParameterValue("mean") / (self$getParameterValue("size") +
          self$getParameterValue("mean")))
    })
  } else if (form == "tbf") {
    ps$addDeps("size", "mean", function(self) {
      self$getParameterValue("size") /
        (1 - self$getParameterValue("prob"))
    })
    ps$addDeps("prob", "mean", function(self) {
      self$getParameterValue("size") /
        (1 - self$getParameterValue("prob"))
    })
    ps$addDeps("qprob", "mean", function(self) {
      self$getParameterValue("size") /
        self$getParameterValue("qprob")
    })
    ps$addDeps("mean", "prob", function(self) {
      (self$getParameterValue("mean") -
        self$getParameterValue("size")) /
        self$getParameterValue("mean")
    })
    ps$addDeps("mean", "qprob", function(self) {
      1 - ((self$getParameterValue("mean") -
        self$getParameterValue("size")) /
        self$getParameterValue("mean"))
    })
    ps$addChecks("mean", function(x, self) x >= self$getParameterValue("size"))
    ps$addChecks("size", function(x, self) x <= self$getParameterValue("mean"))
  } else if (form == "tbs") {
    ps$addDeps("size", "mean", function(self) {
      self$getParameterValue("size") /
        self$getParameterValue("prob")
    })
    ps$addDeps("prob", "mean", function(self) {
      self$getParameterValue("size") /
        self$getParameterValue("prob")
    })
    ps$addDeps("qprob", "mean", function(self) {
      self$getParameterValue("size") /
        (1 - self$getParameterValue("qprob"))
    })
    ps$addDeps("mean", "prob", function(self) {
      self$getParameterValue("size") /
        self$getParameterValue("mean")
    })
    ps$addDeps("mean", "qprob", function(self) {
      1 - (self$getParameterValue("size") /
        self$getParameterValue("mean"))
    })
    ps$addChecks("mean", function(x, self) x >= self$getParameterValue("size"))
    ps$addChecks("size", function(x, self) x <= self$getParameterValue("mean"))
  } else {
    ps$addDeps("size", "mean", function(self) {
      self$getParameterValue("size") *
        (1 - self$getParameterValue("prob")) / self$getParameterValue("prob")
    })
    ps$addDeps("prob", "mean", function(self) {
      self$getParameterValue("size") *
        (1 - self$getParameterValue("prob")) / self$getParameterValue("prob")
    })
    ps$addDeps("qprob", "mean", function(self) {
      self$getParameterValue("size") *
        self$getParameterValue("qprob") / (1 - self$getParameterValue("qprob"))
    })
    ps$addDeps("mean", "prob", function(self) {
      self$getParameterValue("size") /
        (self$getParameterValue("mean") + self$getParameterValue("size"))
    })
    ps$addDeps("mean", "qprob", function(self) {
      1 -
        (self$getParameterValue("size") / (self$getParameterValue("mean") +
          self$getParameterValue("size")))
    })
  }

  return(ps)
}

getParameterSet.Normal <- function(object, mean, var, sd = NULL, prec = NULL) {
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
  ps$addDeps("var", "sd", function(self) self$getParameterValue("var")^0.5)
  ps$addDeps("var", "prec", function(self) self$getParameterValue("var")^-1)
  ps$addDeps("sd", "var", function(self) self$getParameterValue("sd")^2)
  ps$addDeps("sd", "prec", function(self) self$getParameterValue("sd")^-2)
  ps$addDeps("prec", "var", function(self) self$getParameterValue("prec")^-1)
  ps$addDeps("prec", "sd", function(self) self$getParameterValue("prec")^-0.5)

  return(ps)
}

getParameterSet.Pareto <- function(object, shape, scale) {
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Poisson <- function(object, rate) {

  ParameterSet$new(
    id = list("rate"), value = list(1),
    support = list(pos_reals),
    description = list("Arrival Rate")
  )

}

getParameterSet.Rayleigh <- function(object, mode) {

  ParameterSet$new(
    id = list("mode"), value = list(1),
    support = list(pos_reals),
    description = list("Mode - Scale Parameter")
  )

}

getParameterSet.ShiftedLoglogistic <- function(object, scale, shape, location, rate = NULL) { # nolint

  # rate.bool <- scale.bool <- FALSE
  #
  # if (!is.null(rate)) {
  #   rate.bool <- TRUE
  # } else {
  #   scale.bool <- TRUE
  # }

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

  ps$addDeps("rate", "scale", function(self) self$getParameterValue("rate")^-1)
  ps$addDeps("scale", "rate", function(self) self$getParameterValue("scale")^-1)

  return(ps)
}

getParameterSet.StudentT <- function(object, df) {

  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(pos_reals),
    description = list("Degrees of Freedom")
  )

}

getParameterSet.StudentTNoncentral <- function(object, df, location) { # nolint

  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(pos_reals, reals),
    description = list("Degrees of Freedom", "Non-centrality parameter")
  )

}

getParameterSet.Triangular <- function(object, lower, upper, mode, symmetric = FALSE) {

  ps <- ParameterSet$new(
    id = list("lower", "upper", "mode", "symmetric"),
    value = list(0, 1, 0.5, symmetric),
    support = list(reals, reals, reals, LogicalSet$new()),
    settable = list(TRUE, TRUE, !symmetric, FALSE),
    description = list(
      "Lower distribution limit.", "Upper distribution limit.",
      "Distribution mode.", "Type of distribution."
    )
  )

  if (symmetric) {
    ps$addDeps("lower", "mode", function(self) {
      (self$getParameterValue("lower") +
        self$getParameterValue("upper")) / 2
    })
    ps$addDeps("upper", "mode", function(self) {
      (self$getParameterValue("lower") +
        self$getParameterValue("upper")) / 2
    })
  }

  ps$addChecks("lower", function(x, self) x < self$getParameterValue("upper"))
  ps$addChecks("upper", function(x, self) x > self$getParameterValue("lower"))

  return(ps)
}

getParameterSet.Uniform <- function(object, lower, upper) {
  ps <- ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(reals, reals),
    description = list("Lower distribution limit.", "Upper distribution limit.")
  )

  ps$addChecks("lower", function(x, self) x < self$getParameterValue("upper"))
  ps$addChecks("upper", function(x, self) x > self$getParameterValue("lower"))

  return(ps)
}

getParameterSet.Wald <- function(object, mean, shape) {

  ParameterSet$new(
    id = list("mean", "shape"), value = list(1, 1),
    support = list(pos_reals, pos_reals),
    description = list(
      "Mean - Location Parameter",
      "Shape Parameter"
    )
  )
}

getParameterSet.Weibull <- function(object, shape, scale, altscale = NULL) {

  # scale.bool <- altscale.bool <- FALSE
  #
  # if (!is.null(altscale)) {
  #   altscale.bool <- TRUE
  # } else {
  #   scale.bool <- TRUE
  # }

  ps <- ParameterSet$new(
    id = list("shape", "scale", "altscale"), value = list(1, 1, 1),
    support = list(pos_reals, pos_reals, pos_reals),
    description = list("Shape paramer", "Scale parameter", "Alternate scale parameter")
  )

  ps$addDeps("scale", "altscale", function(self) {
    self$getParameterValue("scale")^-self$getParameterValue("shape")
  })
  ps$addDeps("altscale", "scale", function(self) {
    exp(log(self$getParameterValue("altscale")) /
      (-self$getParameterValue("shape")))
  })

  return(ps)
}

getParameterSet.WeightedDiscrete <- function(object, x, pdf, cdf = NULL) { # nolint
  # pdf.bool <- cdf.bool <- FALSE
  #
  # if (!is.null(cdf)) {
  #   cdf.bool <- TRUE
  # } else {
  #   pdf.bool <- TRUE
  # }

  n <- length(x)


  ps <- ParameterSet$new(
    id = list("x", "pdf", "cdf"),
    value = list(x, rep(1, n), rep(1, n)),
    support = list(reals^"n", Interval$new(0, 1)^"n", Interval$new(0, 1)^"n"),
    description = list(
      "Data.", "Probability density function.",
      "Cumulative distribution function."
    )
  )
  ps$addDeps("pdf", "cdf", function(self) cumsum(self$getParameterValue("pdf")))
  ps$addDeps("cdf", "pdf", function(self) {
    c(
      self$getParameterValue("cdf")[1],
      diff(self$getParameterValue("cdf"))
    )
  })
  ps$addChecks("pdf", function(x, self) {
    length(x) == length(self$getParameterValue("x")) &
      all(x >= 0) & all(x <= 1) & sum(x) <= 1
  })
  ps$addChecks("cdf", function(x, self) {
    length(x) == length(self$getParameterValue("x")) &
      all(x >= 0) & all(x <= 1)
  })

  return(ps)
}
