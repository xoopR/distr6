getParameterSet <- function(x, ...){
  UseMethod("getParameterSet", x)
}

getParameterSet.Arcsine <- function(x, lower, upper, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  if(verbose) message("Parameterised with lower and upper.")

  ps <- ParameterSet$new(id = list("lower","upper"), value = list(0, 1),
                         support = list(Reals$new(),Reals$new()),
                         settable = list(TRUE, TRUE), updateFunc = NULL,
                         description = list("Lower distribution limit.",
                                            "Upper distribution limit."))
  return(ps)
}

getParameterSet.Bernoulli <- function(x, prob, qprob = NULL, verbose = FALSE){

  prob.bool = qprob.bool = FALSE

  if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else if(!is.null(prob)){
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                         support = list(Interval$new(0,1), Interval$new(0,1)),
                         settable = list(prob.bool, qprob.bool),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                         description = list("Probability of Success", "Probability of failure"))
  return(ps)
}

getParameterSet.Beta <- function(x, shape1, shape2, verbose = FALSE){

  if(verbose) message("Parameterised with shape1 and shape2.")


  ps <- ParameterSet$new(id = list("shape1","shape2"), value = list(1,1),
                         support = list(PosReals$new(), PosReals$new()),
                         settable = list(TRUE, TRUE),
                         updateFunc = NULL,
                         description = list("Shape Parameter (alpha)","Shape Parameter (beta)"))

  return(ps)
}

getParameterSet.Binomial <- function(x, size, prob, qprob = NULL, verbose = FALSE){

  prob.bool = qprob.bool = FALSE

  if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else {
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("prob","qprob","size"), value = list(0.5, 0.5, 10),
                         support = list(Interval$new(0,1), Interval$new(0,1), PosNaturals$new()),
                         settable = list(prob.bool, qprob.bool, TRUE),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')", NULL),
                         description = list("Probability of Success",
                                            "Probability of failure", "Number of trials"))

  return(ps)
}

getParameterSet.Categorical <- function(x, probs, verbose = FALSE){

  categories = unlist(length(probs))
  ps <- ParameterSet$new(id = list("probs","categories"),
                         value = list(rep(0.5,categories), categories),
                         support = list(Interval$new(0,1), PosNaturals$new()),
                         settable = list(TRUE, FALSE),
                         updateFunc = list(NA, "length(self$getParameterValue('probs'))"),
                         description = list("Probability of success i", "Number of categories"))

  if(verbose) message("Parameterised with probs.")

  return(ps)
}

getParameterSet.Cauchy <- function(x, location, scale, verbose = FALSE){

  if(verbose) message("Parameterised with location and scale.")

  ps <- ParameterSet$new(id = list("location","scale"), value = list(0, 1),
                         support = list(Reals$new(), PosReals$new()),
                         settable = list(TRUE, TRUE),
                         updateFunc = NULL,
                         description = list("Location Parameter",
                                            "Scale Parameter"))
  return(ps)
}

getParameterSet.ChiSquared <- function(x, df, verbose = FALSE){

  if(verbose) message("Parameterised with df.")

  ps <- ParameterSet$new(id = list("df"), value = list(1),
                         support = list(Naturals$new()),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Degrees of Freedom"))

  return(ps)
}

getParameterSet.Degenerate <- function(x, mean, verbose = FALSE){

  checkmate::assert(mean < Inf)
  checkmate::assert(mean > -Inf)

  if(verbose) message("Parameterised with mean.")

  ps <- ParameterSet$new(id = list("mean"), value = list(0),
                         support = list(Reals$new()),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Location Parameter"))

  return(ps)
}

getParameterSet.Dirichlet <- function(x, params, verbose = FALSE){

  K = length(params)
  ps <- ParameterSet$new(id = list("params","K"),
                         value = list(rep(1,K), K),
                         support = list(PosReals$new(), Interval$new(2,Inf,type="[)",class="integer")),
                         settable = list(TRUE, FALSE),
                         updateFunc = list(NA, "length(self$getParameterValue('params'))"),
                         description = list("Conccentration parameters", "Number of categories"))

  if(verbose) message("Parameterised with params.")

  return(ps)
}

getParameterSet.DiscreteUniform <- function(x, lower, upper, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  if(verbose) message("Parameterised with lower and upper.")

  ps <- ParameterSet$new(id = list("lower","upper", "N"),
                         value = list(0, 1, (upper - lower + 1)),
                         support = list(Integers$new(), Integers$new(), Integers$new()),
                         settable = list(TRUE, TRUE, FALSE),
                         updateFunc = list(NULL, NULL,
                                           "self$getParameterValue('upper') - self$getParameterValue('lower') + 1"),
                         description = list("Lower distribution limit.", "Upper distribution limit.",
                                            "Distribution width."))

  return(ps)
}

getParameterSet.Exponential <- function(x, rate, scale = NULL, verbose = FALSE){

  rate.bool = scale.bool = FALSE

  if(!is.null(scale)){
    if(verbose) message("Parameterised with scale.")
    scale.bool = TRUE
  } else{
    if(verbose) message("Parameterised with rate.")
    rate.bool = TRUE
  }

  ps <-  ParameterSet$new(id = list("rate","scale"), value = list(1, 1),
                          support = list(PosReals$new(), PosReals$new()),
                          settable = list(rate.bool, scale.bool),
                          updateFunc = list(NA, "self$getParameterValue('rate')^-1"),
                          description = list("Arrival Rate", "Scale"))

  return(ps)
}

getParameterSet.FDistribution <- function(x, df1, df2, verbose = FALSE){
  if (verbose) message("Parameterised with df1 and df2.")

  ps <- ParameterSet$new(id = list("df1", "df2"), value = list(1, 1),
                         support = list(Naturals$new(), Naturals$new()),
                         settable = list(TRUE, TRUE),
                         updateFunc = list(NA, NA),
                         description = list("Degrees of freedom 1",
                                            "Degrees of freedom 2"))
}

getParameterSet.Frechet <- function(x, shape, scale, minimum, verbose = FALSE){

  if(verbose) message("Parameterised with shape, scale and minimum.")

  ps <- ParameterSet$new(id = list("shape","scale","minimum"), value = list(1, 1, 0),
                         support = list(PosReals$new(), PosReals$new(), Reals$new()),
                         settable = list(TRUE,TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape Parameter","Scale Parameter",
                                            "Distribution Minimum - Location Parameter"))

  return(ps)
}

getParameterSet.Gamma <- function(x, shape, rate, scale = NULL, mean = NULL, verbose = FALSE){

  rate.bool = mean.bool = scale.bool = FALSE

  if(!is.null(mean)){
    if(verbose) message("Parameterised with shape and mean.")
    mean.bool = TRUE
  } else if(!is.null(scale)){
    if(verbose) message("Parameterised with shape and scale.")
    scale.bool = TRUE
  } else{
    if(verbose) message("Parameterised with shape and rate.")
    rate.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("shape","rate","scale","mean"), value = list(1, 1, 1, 1),
                         support = list(PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new()),
                         settable = list(TRUE, rate.bool, scale.bool, mean.bool),
                         updateFunc = list(NA, NA,
                                           "self$getParameterValue('rate')^-1",
                                           "(self$getParameterValue('shape'))/(self$getParameterValue('rate'))"),
                         description = list("Shape - Shape Parameter",
                                            "Rate - Inverse Scale Parameter",
                                            "Scale - Scale Parameter",
                                            "Mean - Mean Parameter"))
  return(ps)
}

getParameterSet.Geometric <- function(x, prob, qprob = NULL, trials = TRUE, verbose = FALSE){

  prob.bool = qprob.bool = FALSE

  if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else{
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }

  if(trials)
    ps <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                           support = list(Interval$new(0,1,type="()"),Interval$new(0,1,type="()")),
                           settable = list(prob.bool, qprob.bool),
                           updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                           description = list("Probability of Success",
                                              "Probability of failure"))
  else
    ps <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                           support = list(Interval$new(0,1,type="(]"),Interval$new(0,1,type="(]")),
                           settable = list(prob.bool, qprob.bool),
                           updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                           description = list("Probability of Success",
                                              "Probability of failure"))

  return(ps)
}

getParameterSet.Gompertz <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <- ParameterSet$new(id = list("shape","scale"), value = list(1, 1),
                         support = list(PosReals$new(), PosReals$new()),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape parameter","Scale parameter"))

  return(ps)
}

getParameterSet.Gumbel <- function(x, location, scale, verbose = FALSE){

  if(verbose) message("Parameterised with location and scale.")

  ps <- ParameterSet$new(id = list("location","scale"), value = list(0, 1),
                         support = list(Reals$new(), PosReals$new()),
                         settable = list(TRUE, TRUE),
                         updateFunc = NULL,
                         description = list("Location Parameter",
                                            "Scale Parameter"))
  return(ps)
}

getParameterSet.Hypergeometric <- function(x, size, successes, failures = NULL, draws, verbose = FALSE){

  successes.bool = failures.bool = FALSE

  if(!is.null(failures)){
    if(verbose) message("Parameterised with number of failures.")
    failures.bool = TRUE
  } else if(!is.null(successes)){
    if(verbose) message("Parameterised with number of successes.")
    successes.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("size","successes","failures","draws"),
                         value = list(1e08, 1e08, 0, 1e08),
                         support = list(Naturals$new(),Naturals$new(),Naturals$new(),Naturals$new()),
                         settable = list(TRUE, successes.bool,failures.bool,TRUE),
                         updateFunc = list(NA,NA,
                                           "self$getParameterValue('size') - self$getParameterValue('successes')",
                                           NA),
                         description = list("Population size",
                                            "Number of successes in the population.",
                                            "Number of failures in the population.",
                                            "Number of draws."))
  return(ps)

}

getParameterSet.InverseGamma <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <- ParameterSet$new(id = list("shape","scale"), value = list(1, 1),
                         support = list(PosReals$new(), PosReals$new()),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape Parameter","Scale Parameter"))

  return(ps)
}

getParameterSet.Laplace <- function(x, mean, scale, var = NULL, verbose = FALSE){

  var.bool = scale.bool = FALSE

  if(!is.null(var)){
    if(verbose) message("Parameterised with mean and var.")
    var.bool = TRUE
  } else{
    if(verbose) message("Parameterised with mean and scale.")
    scale.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("mean","scale","var"), value = list(0, 1, 2),
                         support = list(Reals$new(),PosReals$new(),PosReals$new()),
                         settable = list(TRUE, scale.bool, var.bool),
                         updateFunc = list(NA, NA, "2*self$getParameterValue('scale')^2"),
                         description = list("Mean - Location Parameter",
                                            "Scale - Scale Parameter",
                                            "Variance - Alternate Scale Parameter"))
  return(ps)
}

getParameterSet.Logarithmic <- function(x, theta, verbose = FALSE){

  if(verbose) message("Parameterised with theta.")

  ps <- ParameterSet$new(id = list("theta"), value = list(0.5),
                         support = list(Interval$new(0,1,type="()")),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Theta parameter."))

  return(ps)
}

getParameterSet.Logistic <- function(x, mean, scale, sd = NULL, verbose = FALSE){

  sd.bool = scale.bool = FALSE

  if(!is.null(sd)){
    if(verbose) message("Parameterised with mean and sd.")
    sd.bool = TRUE
  } else{
    if(verbose) message("Parameterised with mean and scale.")
    scale.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("mean","scale","sd"), value = list(0, 1, pi/sqrt(3)),
                         support = list(Reals$new(), PosReals$new(), PosReals$new()),
                         settable = list(TRUE, scale.bool, sd.bool),
                         updateFunc = list(NA,NA,
                                           "self$getParameterValue('scale')*pi/sqrt(3)"),
                         description = list("Mean - Location Parameter",
                                            "Scale - Scale Parameter",
                                            "Standard Deviation - Alternative Scale Parameter"))
  return(ps)
}

getParameterSet.LogLogistic <- function(x, scale, shape, location, verbose = FALSE){

  if(verbose) message("Parameterised with scale, shape and location.")

  ps <- ParameterSet$new(id = list("scale","shape","location"), value = list(1,1,0),
                         support = list(PosReals$new(), PosReals$new(), PosReals$new(zero = T)),
                         settable = list(TRUE, TRUE, TRUE),
                         updateFunc = NULL,
                         description = list("Scale Parameter",
                                            "Shape Parameter",
                                            "Location Parameter"))
  return(ps)
}

getParameterSet.Lognormal <- function(x, meanlog, varlog, sdlog = NULL, preclog = NULL,
                                      mean = NULL, var = NULL, sd = NULL, prec = NULL, verbose = FALSE){

  varlog.bool = sdlog.bool = preclog.bool = var.bool = sd.bool = prec.bool = FALSE
  if(is.null(meanlog) & is.null(mean)){
    if(!is.null(var) | !is.null(sd) | !is.null(prec)){
      if(verbose) message("Meanlog and mean missing, natural scale assumed.")
      mean = 0
    } else{
      if(verbose) message("Meanlog and mean missing, log scale assumed.")
      meanlog = 0
    }

  }

  if(!is.null(meanlog)){
    if(!is.null(preclog)){
      if(verbose) message("Parameterised with meanlog and preclog.")
      preclog.bool = TRUE
    }else if(!is.null(sdlog)){
      if(verbose) message("Parameterised with meanlog and sdlog.")
      sdlog.bool = TRUE
    }else if(!is.null(varlog)){
      if(verbose) message("Parameterised with meanlog and varlog.")
      varlog.bool = TRUE
    }

    ps <- ParameterSet$new(id = list("meanlog","varlog","sdlog","preclog", "mean", "var", "sd", "prec"),
                           value = list(0, 1, 1, 1, exp(0.5), (exp(1)-1)*exp(1), sqrt((exp(1)-1)*exp(1)), ((exp(1)-1)*exp(1))^-1),
                           support = list(Reals$new(), PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new(),
                                          PosReals$new(), PosReals$new(), PosReals$new()),
                           settable = list(TRUE, varlog.bool, sdlog.bool, preclog.bool, FALSE,
                                           var.bool, sd.bool, prec.bool),
                           updateFunc = list(NA, NA, "self$getParameterValue('varlog')^0.5",
                                             "self$getParameterValue('varlog')^-1",
                                             "exp(self$getParameterValue('meanlog') + self$getParameterValue('varlog')/2)",
                                             "(exp(self$getParameterValue('varlog')) - 1) * exp(2 * self$getParameterValue('meanlog') + self$getParameterValue('varlog'))",
                                             "sqrt((exp(self$getParameterValue('varlog')) - 1) * exp(2 * self$getParameterValue('meanlog') + self$getParameterValue('varlog')))",
                                             "((exp(self$getParameterValue('varlog')) - 1) * exp(2 * self$getParameterValue('meanlog') + self$getParameterValue('varlog')))^(-1)"),
                           description = list("meanlog - Location Parameter on log scale",
                                              "varlog - Squared Scale Parameter on log scale",
                                              "sdlog - Scale Parameter on log scale",
                                              "preclog - Inverse Squared Scale Parameter on logscale",
                                              "mean - Location Parameter",
                                              "var - Squared Scale Parameter",
                                              "sd - Scale Parameter",
                                              "prec - Inverse Squared Scale Parameter"))

  } else{
    if(!is.null(prec)){
      if(verbose) message("Parameterised with mean and prec.")
      prec.bool = TRUE
    }else if(!is.null(sd)){
      if(verbose) message("Parameterised with mean and sd.")
      sd.bool = TRUE
    }else if(!is.null(var)){
      if(verbose) message("Parameterised with mean and var.")
      var.bool = TRUE
    }

    ps <- ParameterSet$new(id = list("meanlog","varlog","sdlog","preclog", "mean", "var", "sd", "prec"),
                           value = list(log(1/sqrt(2)), log(2), sqrt(log(2)), 1/log(2), 1, 1, 1, 1),
                           support = list(Reals$new(), PosReals$new(), PosReals$new(), PosReals$new(), PosReals$new(),
                                          PosReals$new(), PosReals$new(), PosReals$new()),
                           settable = list(FALSE, varlog.bool, sdlog.bool, preclog.bool, TRUE,
                                           var.bool, sd.bool, prec.bool),
                           updateFunc = list("log(self$getParameterValue('mean')/sqrt(1 +
                                             self$getParameterValue('var')/self$getParameterValue('mean')^2))",
                                             "log(1 + self$getParameterValue('var')/self$getParameterValue('mean')^2)",
                                             "(log(1 + self$getParameterValue('var')/self$getParameterValue('mean')^2))^0.5",
                                             "(log(1 + self$getParameterValue('var')/self$getParameterValue('mean')^2))^-1",
                                             NA,
                                             NA,
                                             "self$getParameterValue('var')^0.5",
                                             "self$getParameterValue('var')^-1"),
                           description = list("meanlog - Location Parameter on log scale",
                                              "varlog - Squared Scale Parameter on log scale",
                                              "sdlog - Scale Parameter on log scale",
                                              "preclog - Inverse Squared Scale Parameter on logscale",
                                              "meanlog - Location Parameter",
                                              "varlog - Squared Scale Parameter",
                                              "sdlog - Scale Parameter",
                                              "preclog - Inverse Squared Scale Parameter"))
  }



  return(ps)
}

getParameterSet.Multinomial <- function(x, size, probs, verbose = FALSE){

  K = unlist(length(probs))
  ps <- ParameterSet$new(id = list("size","K", "probs"),
                         value = list(1, K, rep(0.5,K)),
                         support = list(PosNaturals$new(), PosNaturals$new(), Interval$new(0,1)),
                         settable = list(TRUE, FALSE, TRUE),
                         updateFunc = list(NA, "length(self$getParameterValue('probs'))",NA),
                         description = list("Number of trials", "Number of categories",
                                            "Probability of success i"))

  if(verbose) message("Parameterised with size and probs.")

  return(ps)
}

getParameterSet.MultivariateNormal <- function(x, mean, cov, prec = NULL, verbose = FALSE){

  cov.bool = prec.bool = FALSE

  if(!is.null(prec)){
    if(verbose) message("Parameterised with mean and prec.")
    prec.bool = TRUE
  } else{
    if(verbose) message("Parameterised with mean and cov.")
    cov.bool = TRUE
  }

  K = length(mean)
  ps <- ParameterSet$new(id = list("mean","cov","prec","K"),
                         value = list(rep(0, K), matrix(rep(0,K^2),nrow=K),
                                      matrix(rep(0,K^2),nrow=K), K),
                         support = list(Reals$new(), Reals$new(dim=2),
                                        Reals$new(dim=2), PosNaturals$new()),
                         settable = list(TRUE, cov.bool, prec.bool, FALSE),
                         updateFunc = list(NA, NA, "solve(matrix(self$getParameterValue('cov'),
                                           nrow = self$getParameterValue('K')))","length(self$getParameterValue('mean'))"),
                         description = list("Vector of means - Location Parameter.",
                                            "Covariance matrix - Scale Parameter.",
                                            "Precision matrix - Scale Parameter.",
                                            "Number of components"))

  return(ps)
}

getParameterSet.NegativeBinomial <- function(x, size, prob, qprob = NULL, mean = NULL, form, verbose = FALSE){

  prob.bool = qprob.bool = mean.bool = FALSE

  if(!is.null(mean)){
    if(verbose) message("Parameterised with size and mean.")
    mean.bool = TRUE
  } else if(!is.null(qprob)){
    if(verbose) message("Parameterised with size and qprob.")
    qprob.bool = TRUE
  } else {
    if(verbose) message("Parameterised with size and prob.")
    prob.bool = TRUE
  }

  if(form == "sbf"){
    updateFunc <- 'self$getParameterValue("size") * self$getParameterValue("prob") / (1-self$getParameterValue("prob"))'
    desc <- "Number of failures"
  } else if(form == "tbf"){
    updateFunc <- 'self$getParameterValue("size") / (1-self$getParameterValue("prob"))'
    desc <- "Number of failures"
  } else if(form == "tbs"){
    updateFunc <- 'self$getParameterValue("size") / self$getParameterValue("prob")'
    desc <- "Number of successes"
  } else {
    updateFunc <- 'self$getParameterValue("size") * (1-self$getParameterValue("prob")) / self$getParameterValue("prob")'
    desc <- "Number of successes"
  }

  ps <- ParameterSet$new(id = list("prob","qprob","mean","size"), value = list(0.5, 0.5, 20, 10),
                         support = list(Interval$new(0,1,type="()"),
                                        Interval$new(0,1,type="()"),
                                        PosReals$new(),
                                        PosNaturals$new()),
                         settable = list(prob.bool, qprob.bool, mean.bool, TRUE),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')", updateFunc, NULL),
                         description = list("Probability of Success",
                                            "Probability of failure",
                                            "Mean - Location Parameter",desc))

  return(ps)
}

getParameterSet.Normal <- function(x, mean, var, sd = NULL, prec = NULL, verbose = FALSE){

  var.bool = sd.bool = prec.bool = FALSE

  if(!is.null(prec)){
    if(verbose) message("Parameterised with mean and prec.")
    prec.bool = TRUE
  } else if(!is.null(sd)){
    if(verbose) message("Parameterised with mean and sd.")
    sd.bool = TRUE
  } else{
    if(verbose) message("Parameterised with mean and var.")
    var.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("mean","var","sd","prec"),
                         value = list(0, 1, 1, 1),
                         support = list(Reals$new(), PosReals$new(), PosReals$new(), PosReals$new()),
                         settable = list(TRUE, var.bool, sd.bool, prec.bool),
                         updateFunc = list(NA, NA, "self$getParameterValue('var')^0.5", "self$getParameterValue('var')^-1"),
                         description = list("Mean - Location Parameter",
                                            "Variance - Squared Scale Parameter",
                                            "Standard Deviation - Scale Parameter",
                                            "Precision - Inverse Squared Scale Parameter"))
  return(ps)
}

getParameterSet.Pareto <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <- ParameterSet$new(id = list("shape","scale"), value = list(1, 1),
                         support = list(PosReals$new(), PosReals$new()),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape parameter","Scale parameter"))

  return(ps)
}

getParameterSet.Poisson <- function(x, rate, verbose = FALSE){

  if(verbose) message("Parameterised with rate.")

  ps <-  ParameterSet$new(id = list("rate"), value = list(1),
                          support = list(PosReals$new()),
                          settable = list(TRUE),
                          updateFunc = list(NA),
                          description = list("Arrival Rate"))

  return(ps)
}

getParameterSet.Rayleigh <- function(x, mode, verbose = FALSE){

  if(verbose) message("Parameterised with mode.")

  ps <-  ParameterSet$new(id = list("mode"), value = list(1),
                          support = list(PosReals$new()),
                          settable = list(TRUE),
                          updateFunc = list(NA),
                          description = list("Mode - Scale Parameter"))

  return(ps)
}

getParameterSet.StudentT <- function(x, df, verbose = FALSE){

  if(verbose) message("Parameterised with df.")

  ps <- ParameterSet$new(id = list("df"), value = list(1),
                         support = list(PosReals$new()),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Degrees of Freedom"))

  return(ps)
}

getParameterSet.Triangular <- function(x, lower, upper, mode, symmetric, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower < upper, .var.name = "lower must be < upper")

  if(symmetric){
    updateFunc = "(self$getParameterValue('lower') + self$getParameterValue('upper'))/2"
    settable = FALSE
    if(verbose) message("Parameterised with lower and upper.")
  } else{
    checkmate::assert(mode >= lower, mode <= upper, combine = "and", .var.name = "mode must be between lower and upper")
    updateFunc = NA
    settable = TRUE
    if(verbose) message("Parameterised with lower, upper and mode.")
  }

  ps <- ParameterSet$new(id = list("lower","upper","mode"),
                         value = list(0, 1, 0.5),
                         support = list(Reals$new(),Reals$new(),Reals$new()),
                         settable = list(TRUE, TRUE, settable),
                         updateFunc = list(NA, NA, updateFunc),
                         description = list("Lower distribution limit.", "Upper distribution limit.",
                                            "Distribution mode."))

  return(ps)
}

getParameterSet.Uniform <- function(x, lower, upper, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower < upper, .var.name = "lower must be < upper")

  if(verbose) message("Parameterised with lower and upper.")

  ps <- ParameterSet$new(id = list("lower","upper"),
                         value = list(0, 1),
                         support = list(Reals$new(), Reals$new()),
                         settable = list(TRUE, TRUE),
                         updateFunc = NULL,
                         description = list("Lower distribution limit.", "Upper distribution limit."))

  return(ps)
}

getParameterSet.Wald <- function(x, mean, shape, verbose = FALSE){

  if(verbose) message("Parameterised with mean and shape.")

  ps <- ParameterSet$new(id = list("mean","shape"), value = list(1, 1),
                         support = list(PosReals$new(), PosReals$new()),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Mean - Location Parameter",
                                            "Shape Parameter"))

  return(ps)
}

getParameterSet.Weibull <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <-  ParameterSet$new(id = list("shape","scale"), value = list(1,1),
                          support = list(PosReals$new(), PosReals$new()),
                          settable = list(TRUE,TRUE),
                          updateFunc = list(NA,NA),
                          description = list("Shape paramer", "Scale parameter"))

  return(ps)
}

