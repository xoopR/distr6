getParameterSet <- function(x, ...) {
  UseMethod("getParameterSet", x)
}

getParameterSet.Arcsine <- function(x, lower, upper) {

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  ParameterSet$new(
    id = list("lower", "upper"), value = list(0, 1),
    support = list(Reals$new(), Reals$new()),
    settable = list(TRUE, TRUE), updateFunc = NULL,
    description = list(
      "Lower distribution limit.",
      "Upper distribution limit."
    )
  )
}

getParameterSet.Bernoulli <- function(x, prob, qprob = NULL) {

  prob.bool <- qprob.bool <- FALSE

  if (!is.null(qprob)) {
    qprob.bool <- TRUE
  } else if (!is.null(prob)) {
    prob.bool <- TRUE
  }

  ParameterSet$new(
    id = list("prob", "qprob"), value = list(0.5, 0.5),
    support = list(Interval$new(0, 1), Interval$new(0, 1)),
    settable = list(prob.bool, qprob.bool),
    updateFunc = list(
      NULL,
      function(self) 1 - self$getParameterValue("prob")
    ),
    description = list("Probability of Success", "Probability of failure")
  )
}

getParameterSet.Beta <- function(x, shape1, shape2) {

  ParameterSet$new(
    id = list("shape1", "shape2"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Shape Parameter (alpha)", "Shape Parameter (beta)")
  )
}

getParameterSet.BetaNoncentral <- function(x, shape1, shape2, location) {
  ParameterSet$new(
    id = list("shape1", "shape2", "location"), value = list(1, 1, 0),
    support = list(PosReals$new(), PosReals$new(), PosReals$new(zero = TRUE)),
    settable = list(TRUE, TRUE, TRUE),
    updateFunc = NULL,
    description = list("Shape Parameter (alpha)", "Shape Parameter (beta)", "Non-centrality parameter")
  )
}

getParameterSet.Binomial <- function(x, size, prob, qprob = NULL) {

  prob.bool <- qprob.bool <- FALSE

  if (!is.null(qprob)) {
    qprob.bool <- TRUE
  } else {
    prob.bool <- TRUE
  }

  ParameterSet$new(
    id = list("prob", "qprob", "size"), value = list(0.5, 0.5, 10),
    support = list(Interval$new(0, 1), Interval$new(0, 1), PosNaturals$new()),
    settable = list(prob.bool, qprob.bool, TRUE),
    updateFunc = list(
      NULL,
      function(self) 1 - self$getParameterValue("prob"), NULL
    ),
    description = list(
      "Probability of Success",
      "Probability of failure", "Number of trials"
    )
  )
}

getParameterSet.Categorical <- function(x, probs) {

  categories <- length(probs)

  ParameterSet$new(
    id = list("probs", "categories"),
    value = list(rep(0.5, categories), categories),
    support = list(setpower(Interval$new(0, 1), categories), PosNaturals$new()),
    settable = list(TRUE, FALSE),
    updateFunc = list(
      NULL,
      function(self) length(self$getParameterValue("probs"))
    ),
    description = list("Probability of success i", "Number of categories")
  )
}

getParameterSet.Cauchy <- function(x, location, scale) {

  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(Reals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
  )
}

getParameterSet.ChiSquared <- function(x, df) {
  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(PosReals$new(zero = TRUE)),
    settable = list(TRUE),
    updateFunc = NULL,
    description = list("Degrees of Freedom")
  )
}

getParameterSet.ChiSquaredNoncentral <- function(x, df, location) {
  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(PosReals$new(zero = TRUE), PosReals$new(zero = TRUE)),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Degrees of Freedom", "Non-centrality parameter")
  )
}

getParameterSet.Degenerate <- function(x, mean) {
  ParameterSet$new(
    id = list("mean"), value = list(0),
    support = list(Reals$new()),
    settable = list(TRUE),
    updateFunc = list(NULL),
    description = list("Location Parameter")
  )
}

getParameterSet.Dirichlet <- function(x, params) {

  K <- length(params)

  ParameterSet$new(
    id = list("params", "K"),
    value = list(rep(1, K), K),
    support = list(setpower(PosReals$new(), K), Interval$new(2, Inf, type = "[)", class = "integer")),
    settable = list(TRUE, FALSE),
    updateFunc = list(
      NULL,
      function(self) length(self$getParameterValue("params"))
    ),
    description = list("Concentration parameters", "Number of categories")
  )
}

getParameterSet.DiscreteUniform <- function(x, lower, upper) {

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  ParameterSet$new(
    id = list("lower", "upper", "N"),
    value = list(0, 1, (upper - lower + 1)),
    support = list(Integers$new(), Integers$new(), Integers$new()),
    settable = list(TRUE, TRUE, FALSE),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("upper") - self$getParameterValue("lower") + 1
    ),
    description = list(
      "Lower distribution limit.", "Upper distribution limit.",
      "Distribution width."
    )
  )

}

getParameterSet.Erlang <- function(x, shape, rate, scale = NULL) {

  rate.bool <- scale.bool <- FALSE

  if (!is.null(scale)) {
    scale.bool <- TRUE
  } else {
    rate.bool <- TRUE
  }

  ParameterSet$new(
    id = list("shape", "rate", "scale"), value = list(1, 1, 1),
    support = list(PosIntegers$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, rate.bool, scale.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("rate")^-1
    ),
    description = list(
      "Shape - Shape Parameter",
      "Rate - Inverse Scale Parameter",
      "Scale - Scale Parameter"
    )
  )
}

getParameterSet.Exponential <- function(x, rate, scale = NULL) {

  rate.bool <- scale.bool <- FALSE

  if (!is.null(scale)) {
    scale.bool <- TRUE
  } else {
    rate.bool <- TRUE
  }

  ParameterSet$new(
    id = list("rate", "scale"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(rate.bool, scale.bool),
    updateFunc = list(
      NULL,
      function(self) self$getParameterValue("rate")^-1
    ),
    description = list("Arrival Rate", "Scale")
  )

}

getParameterSet.FDistribution <- function(x, df1, df2) {

  ParameterSet$new(
    id = list("df1", "df2"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Degrees of freedom 1",
      "Degrees of freedom 2"
    )
  )
}

getParameterSet.FDistributionNoncentral <- function(x, df1, df2, location) {

  ParameterSet$new(
    id = list("df1", "df2", "location"), value = list(1, 1, 0),
    support = list(PosReals$new(), PosReals$new(), PosReals$new(zero = TRUE)),
    settable = list(TRUE, TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Degrees of freedom 1",
      "Degrees of freedom 2",
      "Non-centrality parameter"
    )
  )
}

getParameterSet.Frechet <- function(x, shape, scale, minimum) {

  ParameterSet$new(
    id = list("shape", "scale", "minimum"), value = list(1, 1, 0),
    support = list(PosReals$new(), PosReals$new(), Reals$new()),
    settable = list(TRUE, TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Shape Parameter", "Scale Parameter",
      "Distribution Minimum - Location Parameter"
    )
  )

}

getParameterSet.Gamma <- function(x, shape, rate, scale = NULL, mean = NULL) {

  rate.bool <- mean.bool <- scale.bool <- FALSE

  if (!is.null(mean)) {
    mean.bool <- TRUE
  } else if (!is.null(scale)) {
    scale.bool <- TRUE
  } else {
    rate.bool <- TRUE
  }

  ParameterSet$new(
    id = list("shape", "rate", "scale", "mean"), value = list(1, 1, 1, 1),
    support = list(PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, rate.bool, scale.bool, mean.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("rate")^-1,
      function(self) (self$getParameterValue("shape")) / (self$getParameterValue("rate"))
    ),
    description = list(
      "Shape - Shape Parameter",
      "Rate - Inverse Scale Parameter",
      "Scale - Scale Parameter",
      "Mean - Mean Parameter"
    )
  )
}

getParameterSet.Geometric <- function(x, prob, qprob = NULL, trials = TRUE) {

  prob.bool <- qprob.bool <- FALSE

  if (!is.null(qprob)) {
    qprob.bool <- TRUE
  } else {
    prob.bool <- TRUE
  }

  if (trials) {
    ps <- ParameterSet$new(
      id = list("prob", "qprob"), value = list(0.5, 0.5),
      support = list(Interval$new(0, 1, type = "()"), Interval$new(0, 1, type = "()")),
      settable = list(prob.bool, qprob.bool),
      updateFunc = list(
        NULL,
        function(self) 1 - self$getParameterValue("prob")
      ),
      description = list(
        "Probability of Success",
        "Probability of failure"
      )
    )
  } else {
    ps <- ParameterSet$new(
      id = list("prob", "qprob"), value = list(0.5, 0.5),
      support = list(Interval$new(0, 1, type = "(]"), Interval$new(0, 1, type = "(]")),
      settable = list(prob.bool, qprob.bool),
      updateFunc = list(
        NULL,
        function(self) 1 - self$getParameterValue("prob")
      ),
      description = list(
        "Probability of Success",
        "Probability of failure"
      )
    )
  }

  return(ps)
}

getParameterSet.Gompertz <- function(x, shape, scale) {
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Gumbel <- function(x, location, scale) {
  ParameterSet$new(
    id = list("location", "scale"), value = list(0, 1),
    support = list(Reals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Location Parameter",
      "Scale Parameter"
    )
  )
}

getParameterSet.Hypergeometric <- function(x, size, successes, failures = NULL, draws) {

  successes.bool <- failures.bool <- FALSE

  if (!is.null(failures)) {
    failures.bool <- TRUE
  } else if (!is.null(successes)) {
    successes.bool <- TRUE
  }

  ParameterSet$new(
    id = list("size", "successes", "failures", "draws"),
    value = list(1e08, 1e08, 0, 1e08),
    support = list(Naturals$new(), Naturals$new(), Naturals$new(), Naturals$new()),
    settable = list(TRUE, successes.bool, failures.bool, TRUE),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("size") - self$getParameterValue("successes"),
      NULL
    ),
    description = list(
      "Population size",
      "Number of successes in the population.",
      "Number of failures in the population.",
      "Number of draws."
    )
  )

}

getParameterSet.InverseGamma <- function(x, shape, scale) {

  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Shape Parameter", "Scale Parameter")
  )
}

getParameterSet.Laplace <- function(x, mean, scale, var = NULL) {

  var.bool <- scale.bool <- FALSE

  if (!is.null(var)) {
    var.bool <- TRUE
  } else {
    scale.bool <- TRUE
  }

  ParameterSet$new(
    id = list("mean", "scale", "var"), value = list(0, 1, 2),
    support = list(Reals$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, scale.bool, var.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) 2 * self$getParameterValue("scale")^2
    ),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Variance - Alternate Scale Parameter"
    )
  )
}

getParameterSet.Logarithmic <- function(x, theta) {

  ParameterSet$new(
    id = list("theta"), value = list(0.5),
    support = list(Interval$new(0, 1, type = "()")),
    settable = list(TRUE),
    updateFunc = NULL,
    description = list("Theta parameter.")
  )
}

getParameterSet.Logistic <- function(x, mean, scale, sd = NULL) {

  sd.bool <- scale.bool <- FALSE

  if (!is.null(sd)) {
    sd.bool <- TRUE
  } else {
    scale.bool <- TRUE
  }

  ParameterSet$new(
    id = list("mean", "scale", "sd"), value = list(0, 1, pi / sqrt(3)),
    support = list(Reals$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, scale.bool, sd.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("scale") * pi / sqrt(3)
    ),
    description = list(
      "Mean - Location Parameter",
      "Scale - Scale Parameter",
      "Standard Deviation - Alternative Scale Parameter"
    )
  )
}

getParameterSet.Loglogistic <- function(x, scale, shape, rate = NULL) {

  rate.bool <- scale.bool <- FALSE

  if (!is.null(rate)) {
    rate.bool <- TRUE
  } else {
    scale.bool <- TRUE
  }

  ParameterSet$new(
    id = list("scale", "rate", "shape"), value = list(1, 1, 1),
    support = list(PosReals$new(), PosReals$new(), PosReals$new()),
    settable = list(scale.bool, rate.bool, TRUE),
    updateFunc = list(
      NULL,
      function(self) self$getParameterValue("scale")^-1,
      NULL
    ),
    description = list(
      "Scale Parameter",
      "Rate Parameter",
      "Shape Parameter"
    )
  )
}

getParameterSet.Lognormal <- function(x, meanlog, varlog, sdlog = NULL, preclog = NULL,
                                      mean = NULL, var = NULL, sd = NULL, prec = NULL) {

  varlog.bool <- sdlog.bool <- preclog.bool <- var.bool <- sd.bool <- prec.bool <- FALSE
  if (is.null(meanlog) & is.null(mean)) {
    if (!is.null(var) | !is.null(sd) | !is.null(prec)) {
      mean <- 0
    } else {
      meanlog <- 0
    }

  }

  if (!is.null(meanlog)) {
    if (!is.null(preclog)) {
      preclog.bool <- TRUE
    } else if (!is.null(sdlog)) {
      sdlog.bool <- TRUE
    } else if (!is.null(varlog)) {
      varlog.bool <- TRUE
    }

    ps <- ParameterSet$new(
      id = list("meanlog", "varlog", "sdlog", "preclog", "mean", "var", "sd", "prec"),
      value = list(0, 1, 1, 1, exp(0.5), (exp(1) - 1) * exp(1), sqrt((exp(1) - 1) * exp(1)), ((exp(1) - 1) * exp(1))^-1),
      support = list(
        Reals$new(), PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new(),
        PosReals$new(), PosReals$new(), PosReals$new()
      ),
      settable = list(
        TRUE, varlog.bool, sdlog.bool, preclog.bool, FALSE,
        var.bool, sd.bool, prec.bool
      ),
      updateFunc = list(
        NULL, NULL,
        function(self) self$getParameterValue("varlog")^0.5,
        function(self) self$getParameterValue("varlog")^-1,
        function(self) exp(self$getParameterValue("meanlog") + self$getParameterValue("varlog") / 2),
        function(self) (exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") + self$getParameterValue("varlog")),
        function(self) sqrt((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") + self$getParameterValue("varlog"))),
        function(self) ((exp(self$getParameterValue("varlog")) - 1) * exp(2 * self$getParameterValue("meanlog") + self$getParameterValue("varlog")))^(-1)
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

  } else {
    if (!is.null(prec)) {
      prec.bool <- TRUE
    } else if (!is.null(sd)) {
      sd.bool <- TRUE
    } else if (!is.null(var)) {
      var.bool <- TRUE
    }

    ps <- ParameterSet$new(
      id = list("meanlog", "varlog", "sdlog", "preclog", "mean", "var", "sd", "prec"),
      value = list(log(1 / sqrt(2)), log(2), sqrt(log(2)), 1 / log(2), 1, 1, 1, 1),
      support = list(
        Reals$new(), PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new(),
        PosReals$new(), PosReals$new(), PosReals$new()
      ),
      settable = list(
        FALSE, varlog.bool, sdlog.bool, preclog.bool, TRUE,
        var.bool, sd.bool, prec.bool
      ),
      updateFunc = list(
        function(self) {
          log(self$getParameterValue("mean") / sqrt(1 +
                                                      self$getParameterValue("var") / self$getParameterValue("mean")^2))
        },
        function(self) log(1 + self$getParameterValue("var") / self$getParameterValue("mean")^2),
        function(self) (log(1 + self$getParameterValue("var") / self$getParameterValue("mean")^2))^0.5,
        function(self) (log(1 + self$getParameterValue("var") / self$getParameterValue("mean")^2))^-1,
        NULL,
        NULL,
        function(self) self$getParameterValue("var")^0.5,
        function(self) self$getParameterValue("var")^-1
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
  }

  return(ps)
}

getParameterSet.Multinomial <- function(x, size, probs) {

  K <- unlist(length(probs))
  ParameterSet$new(
    id = list("size", "K", "probs"),
    value = list(1, K, rep(0.5, K)),
    support = list(PosNaturals$new(), PosNaturals$new(), setpower(Interval$new(0, 1), K)),
    settable = list(TRUE, FALSE, TRUE),
    updateFunc = list(
      NULL,
      function(self) length(self$getParameterValue("probs")), NULL
    ),
    description = list(
      "Number of trials", "Number of categories",
      "Probability of success i"
    )
  )
}

getParameterSet.MultivariateNormal <- function(x, mean, cov, prec = NULL) {

  cov.bool <- prec.bool <- FALSE

  if (!is.null(prec)) {
    prec.bool <- TRUE
  } else {
    cov.bool <- TRUE
  }

  K <- length(mean)

  ParameterSet$new(
    id = list("mean", "cov", "prec"),

    value = list(
      rep(0, K),
      matrix(rep(0, K^2), nrow = K),
      matrix(rep(0, K^2), nrow = K)
    ),

    support = list(
      setpower(Reals$new(), K),
      setpower(Reals$new(), K^2),
      setpower(Reals$new(), K^2)
    ),

    settable = list(TRUE, cov.bool, prec.bool),

    updateFunc = list(
      NULL, NULL,
      function(self) {
        list(solve(matrix(self$getParameterValue("cov"),
                     nrow = length(self$getParameterValue("mean")))
        ))
      }),

    description = list(
      "Vector of means - Location Parameter.",
      "Covariance matrix - Scale Parameter.",
      "Precision matrix - Scale Parameter."
    )
  )
}

getParameterSet.NegativeBinomial <- function(x, size, prob, qprob = NULL, mean = NULL, form) {

  prob.bool <- qprob.bool <- mean.bool <- FALSE

  if (!is.null(mean)) {
    mean.bool <- TRUE
  } else if (!is.null(qprob)) {
    qprob.bool <- TRUE
  } else {
    prob.bool <- TRUE
  }

  if (form == "sbf") {
    updateFunc <- function(self) self$getParameterValue("size") * self$getParameterValue("prob") / (1 - self$getParameterValue("prob"))
    desc <- "Number of failures"
  } else if (form == "tbf") {
    updateFunc <- function(self) self$getParameterValue("size") / (1 - self$getParameterValue("prob"))
    desc <- "Number of failures"
  } else if (form == "tbs") {
    updateFunc <- function(self) self$getParameterValue("size") / self$getParameterValue("prob")
    desc <- "Number of successes"
  } else {
    updateFunc <- function(self) self$getParameterValue("size") * (1 - self$getParameterValue("prob")) / self$getParameterValue("prob")
    desc <- "Number of successes"
  }

  ParameterSet$new(
    id = list("prob", "qprob", "mean", "size", "form"),
    value = list(0.5, 0.5, 20, 10, form),
    support = list(
      Interval$new(0, 1, type = "()"),
      Interval$new(0, 1, type = "()"),
      PosReals$new(),
      PosNaturals$new(),
      Set$new("sbf", "tbf", "tbs", "fbs")
    ),
    settable = list(prob.bool, qprob.bool, mean.bool, TRUE, FALSE),
    updateFunc = list(
      NULL,
      function(self) 1 - self$getParameterValue("prob"),
      updateFunc, NULL, NULL
    ),
    description = list(
      "Probability of Success",
      "Probability of failure",
      "Mean - Location Parameter", desc, "Distribution form"
    )
  )
}

getParameterSet.Normal <- function(x, mean, var, sd = NULL, prec = NULL) {

  var.bool <- sd.bool <- prec.bool <- FALSE

  if (!is.null(prec)) {
    prec.bool <- TRUE
  } else if (!is.null(sd)) {
    sd.bool <- TRUE
  } else {
    var.bool <- TRUE
  }

  ParameterSet$new(
    id = list("mean", "var", "sd", "prec"),
    value = list(0, 1, 1, 1),
    support = list(Reals$new(), PosReals$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, var.bool, sd.bool, prec.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("var")^0.5,
      function(self) self$getParameterValue("var")^-1
    ),
    description = list(
      "Mean - Location Parameter",
      "Variance - Squared Scale Parameter",
      "Standard Deviation - Scale Parameter",
      "Precision - Inverse Squared Scale Parameter"
    )
  )
}

getParameterSet.Pareto <- function(x, shape, scale) {
  ParameterSet$new(
    id = list("shape", "scale"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Shape parameter", "Scale parameter")
  )
}

getParameterSet.Poisson <- function(x, rate) {

  ParameterSet$new(
    id = list("rate"), value = list(1),
    support = list(PosReals$new()),
    settable = list(TRUE),
    updateFunc = NULL,
    description = list("Arrival Rate")
  )

}

getParameterSet.Rayleigh <- function(x, mode) {

  ParameterSet$new(
    id = list("mode"), value = list(1),
    support = list(PosReals$new()),
    settable = list(TRUE),
    updateFunc = NULL,
    description = list("Mode - Scale Parameter")
  )

}

getParameterSet.ShiftedLoglogistic <- function(x, scale, shape, location, rate = NULL) {

  rate.bool <- scale.bool <- FALSE

  if (!is.null(rate)) {
    rate.bool <- TRUE
  } else {
    scale.bool <- TRUE
  }

  ParameterSet$new(
    id = list("scale", "rate", "shape", "location"), value = list(1, 1, 1, 0),
    support = list(PosReals$new(), PosReals$new(), Reals$new(), Reals$new()),
    settable = list(scale.bool, rate.bool, TRUE, TRUE),
    updateFunc = list(
      NULL,
      function(self) self$getParameterValue("scale")^-1,
      NULL, NULL
    ),
    description = list(
      "Scale Parameter",
      "Rate Parameter",
      "Shape Parameter",
      "Location Parameter"
    )
  )
}

getParameterSet.StudentT <- function(x, df) {

  ParameterSet$new(
    id = list("df"), value = list(1),
    support = list(PosReals$new()),
    settable = list(TRUE),
    updateFunc = NULL,
    description = list("Degrees of Freedom")
  )

}

getParameterSet.StudentTNoncentral <- function(x, df, location) {

  ParameterSet$new(
    id = list("df", "location"), value = list(1, 0),
    support = list(PosReals$new(), Reals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Degrees of Freedom", "Non-centrality parameter")
  )

}

getParameterSet.Triangular <- function(x, lower, upper, mode, symmetric) {

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower < upper, .var.name = "lower must be < upper")

  if (symmetric) {
    updateFunc <- function(self) (self$getParameterValue("lower") + self$getParameterValue("upper")) / 2
    settable <- FALSE
  } else {
    checkmate::assert(mode >= lower, mode <= upper, combine = "and", .var.name = "mode must be between lower and upper")
    updateFunc <- NULL
    settable <- TRUE
  }

  ParameterSet$new(
    id = list("lower", "upper", "mode"),
    value = list(0, 1, 0.5),
    support = list(Reals$new(), Reals$new(), Reals$new()),
    settable = list(TRUE, TRUE, settable),
    updateFunc = list(NULL, NULL, updateFunc),
    description = list(
      "Lower distribution limit.", "Upper distribution limit.",
      "Distribution mode."
    )
  )
}

getParameterSet.Uniform <- function(x, lower, upper) {

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower < upper, .var.name = "lower must be < upper")

  ParameterSet$new(
    id = list("lower", "upper"),
    value = list(0, 1),
    support = list(Reals$new(), Reals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list("Lower distribution limit.", "Upper distribution limit.")
  )
}

getParameterSet.Wald <- function(x, mean, shape) {

  ParameterSet$new(
    id = list("mean", "shape"), value = list(1, 1),
    support = list(PosReals$new(), PosReals$new()),
    settable = list(TRUE, TRUE),
    updateFunc = NULL,
    description = list(
      "Mean - Location Parameter",
      "Shape Parameter"
    )
  )
}

getParameterSet.Weibull <- function(x, shape, scale, altscale) {

  scale.bool <- altscale.bool <- FALSE

  if (!is.null(altscale)) {
    altscale.bool <- TRUE
  } else {
    scale.bool <- TRUE
  }

  ParameterSet$new(
    id = list("shape", "scale", "altscale"), value = list(1, 1, 1),
    support = list(PosReals$new(), PosReals$new(), PosReals$new()),
    settable = list(TRUE, scale.bool, altscale.bool),
    updateFunc = list(
      NULL, NULL,
      function(self) self$getParameterValue("scale")^-self$getParameterValue("shape")
    ),
    description = list("Shape paramer", "Scale parameter", "Alternate scale parameter")
  )
}

getParameterSet.WeightedDiscrete <- function(x, data, pdf = NULL, cdf = NULL) {

  n <- length(data)

  ParameterSet$new(
    id = list("data", "pdf", "cdf"),
    value = list(rep(1, n), rep(1, n), rep(1, n)),
    support = list(Reals$new()^n, Interval$new(0, 1)^n, Interval$new(0, 1)^n),
    settable = list(FALSE, FALSE, FALSE),
    updateFunc = list(NULL, NULL, NULL),
    description = list(
      "Data", "Probability Density Function",
      "Cumulative Distribution Function"
    )
  )
}
