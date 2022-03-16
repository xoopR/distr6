trafo_probs <- function(x, self) {
  probs <- list_element(x, "prob")
  qprobs <- list_element(x, "qprob")

  if (length(probs) && length(qprobs)) {
    stop("Can't update 'prob' and 'qprob' parameters simultaneously")
  }
  if (length(probs)) {
    old <- probs
    old_name <- "prob"
    new_name <- "qprob"
  } else {
    old <- qprobs
    old_name <- "qprob"
    new_name <- "prob"
  }
  c(x,
    setNames(
      as.list(1 - unlist(old)),
      gsub(old_name, new_name, names(old))
    )
  )
}

trafo_rate <- function(x, self) {
  scales <- list_element(x, "scale")
  rates <- list_element(x, "rate")
  if (length(scales) && length(rates)) {
    stop("Can't update 'scale' and 'rate' parameters simultaneously")
  }
  if (length(scales)) {
    old <- scales
    old_name <- "scale"
    new_name <- "rate"
  } else {
    old <- rates
    old_name <- "rate"
    new_name <- "scale"
  }
  c(x,
    setNames(as.list(1 / unlist(old)), gsub(old_name, new_name, names(old))))
}

trafo_normalise <- function(x, self) {
  which <- grepl("probs", names(x))
  if (any(which)) {
    x[which] <- lapply(x[which], function(.x) .x / sum(.x))
  }
  x
}

getParameterSet <- function(object, ...) {
  UseMethod("getParameterSet", object)
}

getParameterSet.Arcsine <- function(object, ...) {
  pset(
    prm("lower", "reals", 0, "required"),
    prm("upper", "reals", 1, "required"),
    deps = list(
      list(id = "lower", on = "upper", cond = cnd("leq", id = "upper"))
    )
  )
}

getParameterSet.Bernoulli <- function(object, ...) {
  pset(
    prm("prob", Interval$new(0, 1), 0.5, c("linked", "required")),
    prm("qprob", Interval$new(0, 1), tags = c("linked", "required")),
    trafo = trafo_probs
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
    trafo = trafo_probs
  )
}

getParameterSet.Categorical <- function(object, ...) {
  pset(
    prm("elements", "universal", 1, tags = "required"),
    prm("probs", Interval$new(0, 1)^"n", 1, tags = "required"),
    deps = list(
      list(id = "probs", on = "elements", cond = cnd("len", id = "elements"))
    ),
    trafo = trafo_normalise
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
      list(id = "lower", on = "upper", cond = cnd("lt", id = "upper"))
    )
  )
}

getParameterSet.Erlang <- function(object, ...) {
  pset(
    prm("shape", "posintegers", 1, tags = "required"),
    prm("rate", "posreals", 1, , tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
    trafo = trafo_rate
  )
}

getParameterSet.Exponential <- function(object, ...) {
  pset(
    prm("rate", "posreals", 1, tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
    trafo = trafo_rate
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
      rates <- scales <- means <- NULL

      shapes <- list_element(x, "shape")

      if (any(grepl("scale", names(x)))) {
        scales <- list_element(x, "scale")
        rates <- setNames(as.list(1 / unlist(scales)),
                          gsub("scale", "rate", names(scales)))
      } else if (any(grepl("mean", names(x)))) {
        means <- list_element(x, "mean")
        rates <- setNames(as.list(unlist(shapes) / unlist(means)),
                          gsub("shape", "rate", names(shapes)))
      }

      if (is.null(rates)) {
        rates <- list_element(x, "rate")
      }

      if (is.null(scales)) {
        scales <- setNames(as.list(1 / unlist(rates)),
                           gsub("rate", "scale", names(rates)))
      }

      if (is.null(means)) {
        means <- setNames(as.list(unlist(shapes) * unlist(scales)),
                          gsub("rate", "mean", names(rates)))
      }

      unique_nlist(c(shapes, rates, scales, means, x))
    }
  )
}

getParameterSet.Geometric <- function(object, trials = FALSE, ...) {
  type <- ifelse(trials, "()", "(]")
  pset(
    prm("prob", Interval$new(0, 1, type = type), 0.5, tags = c("linked", "required")),
    prm("qprob", Interval$new(0, 1, type = type), tags = c("linked", "required")),
    prm("trials", "logicals", trials, tags = "immutable"),
    trafo = trafo_probs
  )
}

getParameterSet.Gompertz <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("scale", "posreals", 1, tags = "required")
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
      sizes <- list_element(x, "size")
      successess <- list_element(x, "successes")
      failuress <- list_element(x, "failures")
      if (length(successess) && length(failuress)) {
        stop("Can't update 'successes' and 'failures' parameters simultaneously")
      } else if (length(successess)) {
        old <- successess
        old_name <- "successes"
        new_name <- "failures"
      } else {
        old <- failuress
        old_name <- "failures"
        new_name <- "successes"
      }

      c(x,
        setNames(as.list(unlist(sizes) - unlist(old)),
                 gsub(old_name, new_name, names(old)))
      )
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
        scales <- list_element(x, "scale")
        vars <- list_element(x, "var")
        if (length(scales) && length(vars)) {
          stop("Can't update 'scale' and 'var' parameters simultaneously")
        }
        if (length(scales)) {
          vars <- setNames(2 * unlist(scales)^2,
                           gsub("scale", "var", names(scales)))
        } else {
          scales <- setNames(sqrt(vars / 2),
                             gsub("var", "scale", names(vars)))
        }

        unique_nlist(c(vars, scales, x))
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
    prm("sd", "posreals", tags = c("linked", "required")),
    trafo = function(x, self) {
      scales <- list_element(x, "scale")
      sds <- list_element(x, "sd")
      if (length(scales) && length(sds)) {
        stop("Can't update 'scale' and 'sd' parameters simultaneously")
      }
      if (length(scales)) {
        sds <- setNames(as.list(unlist(scales) * pi / sqrt(3)),
                        gsub("scale", "sd", names(scales)))
      } else {
        scales <- setNames(as.list(unlist(sds) * sqrt(3) / pi),
                        gsub("scale", "sd", names(scales)))
      }

      unique_nlist(c(scales, sds, x))
    }
  )
}

getParameterSet.Loglogistic <- function(object, ...) {
  pset(
    prm("shape", "posreals", 1, tags = "required"),
    prm("rate", "posreals", 1, tags = c("linked", "required")),
    prm("scale", "posreals", tags = c("linked", "required")),
    trafo = trafo_rate
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
      if (log_vals %nin% c(0, length(x))) {
        stop("Can't update log and non-log parameters simultaneously")
      }

      # first compute var or varlog if other scale params given
      sdlogs <- list_element(x, "sdlog")
      preclogs <- list_element(x, "preclog")
      varlogs <- list_element(x, "varlog")
      sds <- list_element(x, "sd")
      precs <- list_element(x, "prec")
      vars <- list_element(x, "var")

      if (length(sdlogs)) {
        varlogs <- as_named_list(unlist(sdlogs)^2,
                                 gsub("sdlog", "varlog", names(sdlogs)))
      } else if (length(preclogs)) {
        varlogs <- as_named_list(1 / unlist(preclogs),
                                 gsub("preclog", "varlog", names(preclogs)))
      } else if (length(sds)) {
        vars <- as_named_list(unlist(sds)^2, gsub("sd", "var", names(sds)))
      } else if (length(precs)) {
        vars <- as_named_list(1 / unlist(precs),
                              gsub("prec", "var", names(precs)))
      }

      if (log_vals > 0) {
        # calculate mean and var
        meanlogs <- list_element(x, "meanlog")
        means <- as_named_list(exp(unlist(meanlogs) + unlist(varlogs) / 2),
                               gsub("meanlog", "mean", names(meanlogs)))
        vars <- as_named_list(
          (exp(unlist(varlogs)) - 1) *
            exp(2 * unlist(meanlogs) + unlist(varlogs)),
          gsub("varlog", "var", names(varlogs)))
      } else {
        means <- list_element(x, "mean")
        meanlogs <- as_named_list(
          log(unlist(means) / sqrt(1 + unlist(vars) / unlist(means)^2)),
          gsub("mean", "meanlog", names(means))
        )
        varlogs <- as_named_list(
          log(1 + unlist(vars) / unlist(means)^2),
          gsub("var", "varlog", names(vars))
        )
      }

      sdlogs <- as_named_list(sqrt(unlist(varlogs)),
                              gsub("varlog", "sdlog", names(varlogs)))
      preclogs <- as_named_list(1 / unlist(varlogs),
                                gsub("varlog", "preclog", names(varlogs)))
      sds <- as_named_list(sqrt(unlist(vars)),
                           gsub("var", "sd", names(vars)))
      precs <- as_named_list(1 / unlist(vars),
                             gsub("var", "prec", names(vars)))


      unique_nlist(
        c(meanlogs, varlogs, sdlogs, preclogs, means, vars, sds, precs, x)
      )
    }
  )
}

getParameterSet.Multinomial <- function(object, ...) {
  pset(
    prm("size", "posnaturals", 10, tags = "required"),
    prm("probs", Interval$new(0, 1)^"n", rep(0.5, 2), tags = "required"),
    trafo = trafo_normalise
  )
}

getParameterSet.MultivariateNormal <- function(object, ...) { # nolint
  pset(
    prm("mean", "nreals", rep(0, 2), tags = "required"),
    prm("cov", "nreals", matrix(c(1, 0, 0, 1), nrow = 2), tags = c("required", "linked")),
    prm("prec", "nreals", tags = c("required", "linked")),
    trafo = function(x, self) {
      mean <- list_element(x, "mean")
      covs <- list_element(x, "cov")
      precs <- list_element(x, "prec")

      if (length(covs) && length(precs)) {
        stop("Can't update 'cov' and 'prec' parameters simultaneously")
      } else if (length(covs)) {
        old <- covs
        old_name <- "cov"
        new_name <- "prec"
      } else {
        old <- precs
        old_name <- "prec"
        new_name <- "cov"
      }

      old <- Map(function(.x, .y) matrix(.x, length(.y), length(.y)),
                 old, mean)
      new <- setNames(lapply(old, solve), gsub(old_name, new_name, names(old)))
      unique_nlist(c(mean, old, new, x))
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

      forms <- list_element(self$values, "form")
      form <- forms[[1]]
      sizes <- list_element(x, "size")
      probs <- list_element(x, "prob")
      qprobs <- list_element(x, "qprob")
      means <- list_element(x, "mean")

      if (sum(length(probs) > 0, length(qprobs) > 0, length(means) > 0) > 1) {
        stop("Can't update 'prob', 'qprob', 'mean' parameters simultaneously")
      } else if (length(qprobs)) {
        probs <- as_named_list(1 - unlist(qprobs),
                            gsub("qprob", "prob", names(qprobs)))
      } else if (length(means)) {
        probs <- as_named_list(switch(form,
          "sbf" = unlist(means) / (unlist(sizes) + unlist(means)),
          "tbf" = (unlist(means) - unlist(sizes)) / unlist(means),
          "tbs" = unlist(sizes) / unlist(means),
          "fbs" = unlist(sizes) / (unlist(means) + unlist(sizes))
        ), gsub("mean", "prob", names(means)))
      }

      if (!length(qprobs)) {
        qprobs <- as_named_list(1 - unlist(probs),
                              gsub("prob", "qprob", names(probs)))
      }

      if (!length(means)) {
        means <- as_named_list(switch(form,
          "sbf" = unlist(sizes) * (unlist(probs) / unlist(qprobs)),
          "tbf" = unlist(sizes) / unlist(qprobs),
          "tbs" = unlist(sizes) / unlist(probs),
          "fbs" = unlist(sizes) * (unlist(qprobs) / unlist(probs))
        ), gsub("prob", "mean", names(probs)))
      }

      unique_nlist(c(forms, sizes, probs, qprobs, means, x))
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

      vars <- sds <- precs <- NULL

      if (any(grepl("sd", names(x)))) {
        sds <- list_element(x, "sd")
        vars <- setNames(as.list(unlist(sds) ^ 2),
                         gsub("sd", "var", names(sds)))
      } else if (any(grepl("prec", names(x)))) {
        precs <- list_element(x, "prec")
        vars <- setNames(as.list(1 / unlist(precs)),
                         gsub("prec", "var", names(precs)))
      }

      if (is.null(vars)) {
        vars <- list_element(x, "var")
      }

      if (is.null(sds)) {
        sds <- setNames(as.list(sqrt(unlist(vars))),
                          gsub("var", "sd", names(vars)))
      }

      if (is.null(precs)) {
        precs <- setNames(as.list(1 / unlist(vars)),
                          gsub("var", "prec", names(vars)))
      }

      unique_nlist(c(vars, sds, precs, x))
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
    trafo = trafo_rate
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
      list(id = "mode", on = "lower", cond = cnd("gt", id = "lower")),
      list(id = "mode", on = "upper", cond = cnd("lt", id = "upper"))
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
      list(id = "lower", on = "upper", cond = cnd("lt", id = "upper"))
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
      shapes <- list_element(x, "shape")
      scales <- list_element(x, "scale")
      altscales <- list_element(x, "altscale")
      if (length(scales)) {
        altscales <- setNames(as.list(unlist(scales)^-unlist(shapes)),
                              gsub("scale", "altscale", names(scales)))
      } else {
        scales <-
          setNames(as.list(exp(log(unlist(altscales)) / -unlist(shapes))),
                   gsub("scale", "altscale", names(scales)))
      }

      unique_nlist(c(scales, altscales, shapes, x))
    }
  )
}

getParameterSet.WeightedDiscrete <- function(object, ...) { # nolint
  pset(
    prm("x", "nreals", 1, tags = c("required", "unique")),
    prm("pdf", Interval$new(0, 1)^"n", tags = c("required", "linked")),
    prm("cdf", Interval$new(0, 1)^"n", 1, tags = c("required", "linked")),
    deps = list(
      list(id = "cdf", on = "x", cond = cnd("len", id = "x")),
      list(id = "cdf", cond = cnd("inc"))
    ),
    trafo = function(x, self) {
      pdfs <- list_element(x, "pdf")
      cdfs <- list_element(x, "cdf")

      if (length(pdfs)) {
        cdfs <- setNames(
          lapply(pdfs, cumsum),
          gsub("pdf", "cdf", names(pdfs))
        )
      } else {
        pdfs <- setNames(
          lapply(cdfs, function(.x) c(.x[1], diff(.x))),
          gsub("cdf", "pdf", names(cdfs))
        )
      }

      ## FIX FOR M1
      cdfs <- lapply(cdfs, round, digits = 15L)

      unique_nlist(c(pdfs, cdfs, x))
    }
  )
}

getParameterSet.Matdist <- function(object, ...) { # nolint
  pset(
    prm("pdf", Interval$new(0, 1)^"n", tags = c("required", "linked")),
    prm("cdf", Interval$new(0, 1)^"n",
        matrix(0.5, 2, 2, dimnames = list(NULL, 1:2)),
        tags = c("required", "linked")),
    prm("x", "integers", tags = "immutable"),
    trafo = function(x, self) {

      pdf <- list_element(x, "pdf")$pdf
      cdf <- list_element(x, "cdf")$cdf

      if (length(pdf)) {
        cdf <- t(apply(pdf, 1, cumsum))
      } else {
        pdf <- t(apply(cdf, 1, function(.y) c(.y[1], diff(.y))))
      }

      ## FIX FOR M1
      cdf <- round(cdf, digits = 14L)
      pdf <- round(pdf, digits = 14L)

      assert_cdf_matrix(cdf)

      list(pdf = pdf, cdf = cdf, x = as.numeric(colnames(pdf)))
    }
  )
}
