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
    prm("prob", Interval$new(0, 1), 0.5, tags = c("linked", "required")),
    prm("qprob", Interval$new(0, 1), tags = c("linked", "required")),
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
  pset(
    prm("meanlog", "reals", 0, tags = c("required", "means")),
    prm("varlog", "posreals", 1, tags = c("required", "vars")),
    prm("sdlog", "posreals", tags = c("required", "vars")),
    prm("preclog", "posreals", tags = c("required", "vars")),
    prm("mean", "posreals", tags = c("required", "means")),
    prm("var", "posreals", tags = c("required", "vars")),
    prm("sd", "posreals", tags = c("required", "vars")),
    prm("prec", "posreals", tags = c("required", "vars")),
    tag_properties = list(linked = c("means", "vars")),
    trafo = function(x, self) {
      log_vals <- sum(grepl("log", names(x)))
      if (length(x) == 2 && log_vals %nin% c(0, 2)) {
        stop("Can't change one log and non-log parameter simultaneously")
      }

      # first compute var or varlog if other scale params given
      if (!is.null(x$sdlog)) {
        x$varlog <- x$sdlog^2
      } else if (!is.null(x$preclog)) {
        x$varlog <- 1 / x$preclog
      } else if (!is.null(x$sd)) {
        x$var <- x$sd^2
      } else if (!is.null(x$prec)) {
        x$var <- 1 / x$prec
      }

      if (log_vals > 0) {
        # only meanlog updated, use current varlog value
        if (is.null(x$varlog)) {
          x$varlog <- self$values$varlog
        }
        # only varlog updated, use current meanlog value
        if (is.null(x$meanlog)) {
          x$meanlog <- self$values$meanlog
        }
        # calculate mean and var
        x$mean <- exp(x$meanlog + x$varlog / 2)
        x$var <- (exp(x$varlog) - 1) * exp(2 * x$meanlog + x$varlog)
      } else {
        if (is.null(x$var)) {
          x$var <- self$values$var
        }
        if (is.null(x$mean)) {
          x$mean <- self$values$mean
        }
        x$meanlog <- log(x$mean / sqrt(1 + x$var / x$mean^2))
        x$varlog <- log(1 + x$var / x$mean^2)
      }

      x$sdlog <- sqrt(x$varlog)
      x$preclog <- 1 / x$varlog
      x$sd <- sqrt(x$var)
      x$prec <- 1 / x$var

      x
    }
  )
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
  pset(
    prm("mean", "nreals", rep(0, 2), tags = "required"),
    prm("cov", "nreals", matrix(c(1, 0, 0, 1), nrow = 2), tags = c("required", "linked")),
    prm("prec", "nreals", tags = c("required", "linked")),
    trafo = function(x, self) {
      mean <- if (!is.null(x$mean)) x$mean else self$values$mean
      if (!is.null(x$cov)) {
        x$cov <- matrix(x$cov, nrow = length(mean))
        x$prec <- solve(x$cov)
      } else if (!is.null(x$prec)) {
        x$prec <- matrix(x$cov, nrow = length(mean))
        x$cov <- solve(matrix(x$prec, nrow = length(mean)))
      }
      x
    }
  )
}

getParameterSet.NegativeBinomial <- function(object, form = "fbs", ...) { # nolint
  ps <- pset(
    prm("prob", Interval$new(0, 1, type = "()"), 0.5, tags = c("required", "linked")),
    prm("qprob", Interval$new(0, 1, type = "()"), tags = c("required", "linked")),
    prm("mean", "posreals", tags = c("required", "linked")),
    prm("size", "posnaturals", 10, tags = "required"),
    prm("form", Set$new("sbf", "tbf", "tbs", "fbs"), form, tags = c("required", "immutable")),
    trafo = function(x, self) {
      form <- self$values$form
      if (is.null(x$size)) {
        x$size <- self$values$size
      }
      if (is.null(x$prob) && is.null(x$qprob) && is.null(x$mean)) {
        x$prob <- self$values$prob # hold prob constant
      } else if (!is.null(x$qprob)) {
        x$prob <- 1 - x$qprob
      } else if (!is.null(x$mean)) {
        x$prob <- switch(form,
          "sbf" = x$mean / (x$size + x$mean),
          "tbf" = (x$mean - x$size) / x$mean,
          "tbs" = x$size / x$mean,
          "fbs" = x$size / (x$mean + x$size)
        )
      }
      x$qprob <- 1 - x$prob
      x$mean <- switch(form,
          "sbf" = x$size * (x$prob / x$qprob),
          "tbf" = x$size / x$qprob,
          "tbs" = x$size / x$prob,
          "fbs" = x$size * (x$qprob / x$prob)
      )
      x
    }
  )

  if (form %in% c("tbf", "tbs")) {
    ps$add_dep("mean", "size", cnd("geq", id = "size"))
  }

  ps
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
  pset(
    prm("x", "nreals", 1, tags = c("required", "unique")),
    prm("pdf", Interval$new(0, 1)^"n", tags = c("required", "linked")),
    prm("cdf", Interval$new(0, 1)^"n", 1, tags = c("required", "linked")),
    deps = list(
      list(id = "cdf", on = "x", cnd = cnd("len", id = "x"))
    ),
    trafo = function(x, self) {
      if (!is.null(x$pdf)) {
        x$cdf <- cumsum(x$pdf)
      } else if (!is.null(x$cdf)) {
        x$pdf <- c(x$cdf[1], diff(x$cdf))
      }
      x
    }
  )
}
