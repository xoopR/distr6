getParameterSet <- function(x, ...){
  UseMethod("getParameterSet", x)
}

getParameterSet.Normal <- function(x, mean, var, sd = NULL, prec = NULL, verbose = FALSE){

  var.bool = sd.bool = prec.bool = FALSE

  if(!is.null(sd)){
    if(verbose) message("Parameterised with mean and sd.")
    sd.bool = TRUE
  } else if(!is.null(prec)){
    if(verbose) message("Parameterised with mean and prec.")
    prec.bool = TRUE
  } else{
    if(verbose) message("Parameterised with mean and var.")
    var.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("mean","var","sd","prec"), value = list(0, 1, 1, 1),
                         lower = list(-Inf, 0, 0, 0), upper = list(Inf, Inf, Inf, Inf),
                         class = list("numeric","numeric","numeric","numeric"),
                         settable = list(TRUE, var.bool, sd.bool, prec.bool),
                         updateFunc = list(NA, NA, "self$getParameterValue('var')^0.5", "self$getParameterValue('var')^-1"),
                         description = list("Mean - Location Parameter",
                                            "Variance - Squared Scale Parameter",
                                            "Standard Deviation - Scale Parameter",
                                            "Precision - Inverse Squared Scale Parameter"))
  return(ps)
}

getParameterSet.Arcsine <- function(x, lower, upper, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  if(verbose) message("Parameterised with lower and upper.")

  ps <- ParameterSet$new(id = list("lower","upper"), value = list(0, 1),
                         lower = list(-Inf, -Inf), upper = list(Inf, Inf),
                         class = list("numeric","numeric"),
                         settable = list(TRUE, TRUE), updateFunc = NULL,
                         description = list("Lower distribution limit.",
                                            "Upper distribution limit."))
  return(ps)
}

getParameterSet.Bernoulli <- function(x, prob = NULL, qprob = NULL, verbose = FALSE){

  prob.bool = qprob.bool = FALSE

  if(is.null(prob) & is.null(qprob)){
    if(verbose) message("prob and qprob missing. Parameterised with prob = 0.5.")
    prob.bool = TRUE
  } else if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else if(!is.null(prob)){
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                         lower = list(0, 0), upper = list(1, 1),
                         class = list("numeric","numeric"),
                         settable = list(prob.bool, qprob.bool),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                         description = list("Probability of Success", "Probability of failure"))
  return(ps)
}

getParameterSet.Binomial <- function(x, size, prob = NULL, qprob = NULL, verbose = FALSE){

  prob.bool = qprob.bool = FALSE

  if(is.null(prob) & is.null(qprob)){
    if(verbose) message("prob and qprob missing. Parameterised with prob = 0.5.")
    prob.bool = TRUE
  } else if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else if(!is.null(prob)){
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("prob","qprob","size"), value = list(0.5, 0.5, 10),
                         lower = list(0, 0, 1), upper = list(1, 1, Inf),
                         class = list("numeric","numeric","integer"),
                         settable = list(prob.bool, qprob.bool, TRUE),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')", NULL),
                         description = list("Probability of Success",
                                            "Probability of failure", "Number of trials"))

  return(ps)
}

getParameterSet.Degenerate <- function(x, mean, verbose = FALSE){

  checkmate::assert(mean < Inf)
  checkmate::assert(mean > -Inf)

  if(verbose) message("Parameterised with mean.")

  ps <- ParameterSet$new(id = list("mean"), value = list(0),
                         lower = list(-Inf), upper = list(Inf),
                         class = list("numeric"),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Location Parameter"))

  return(ps)
}

getParameterSet.DiscreteUniform <- function(x, lower, upper, verbose = FALSE){

  checkmate::assert(lower > -Inf, upper < Inf, combine = "and", .var.name = "lower and upper must be finite")
  checkmate::assert(lower <= upper, .var.name = "lower must be <= upper")

  if(verbose) message("Parameterised with lower and upper.")

  ps <- ParameterSet$new(id = list("lower","upper", "N"),
                         value = list(0, 1, (upper - lower + 1)),
                         lower = list(-Inf, -Inf, -Inf),
                         upper = list(Inf, Inf, Inf),
                         class = list("integer","integer","integer"),
                         settable = list(TRUE, TRUE, FALSE),
                         updateFunc = list(NULL, NULL,
                                           "self$getParameterValue('upper') - self$getParameterValue('lower') + 1"),
                         description = list("Lower distribution limit.", "Upper distribution limit.",
                                            "Distribution width."))

  return(ps)
}

getParameterSet.Exponential <- function(x, rate, scale = NULL, var = NULL, sd = NULL, prec = NULL, verbose = FALSE){

  rate.bool = scale.bool = FALSE

  if(!is.null(scale)){
    if(verbose) message("Parameterised with scale.")
    scale.bool = TRUE
  } else{
    if(verbose) message("Parameterised with rate.")
    rate.bool = TRUE
  }

  ps <-  ParameterSet$new(id = list("rate","scale"), value = list(1, 1),
                          lower = list(0, 0), upper = list(Inf, Inf),
                          class = list("numeric","numeric"),
                          settable = list(rate.bool, scale.bool),
                          updateFunc = list(NA, "self$getParameterValue('rate')^-1"),
                          description = list("Arrival Rate", "Scale"))

  return(ps)
}

getParameterSet.Gompertz <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <- ParameterSet$new(id = list("shape","scale"), value = list(1, 1),
                         lower = list(0, 0), upper = list(Inf, Inf),
                         class = list("numeric","numeric"),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape parameter","Scale parameter"))

  return(ps)
}

getParameterSet.Multinomial <- function(x, size, probs, verbose = FALSE){

  K = unlist(length(probs))
  ps <- ParameterSet$new(id = list("size","K", "probs"),
                         value = list(1, K, rep(0.5,K)),
                         lower = list(1, 1, 0),
                         upper = list(Inf, Inf, 1),
                         class = list("integer", "integer", "numeric"),
                         settable = list(TRUE, FALSE, TRUE),
                         updateFunc = list(NA, "length(self$getParameterValue('probs'))",NA),
                         description = list("Number of trials", "Number of categories",
                                            "Probability of success i"))

  if(verbose) message("Parameterised with size and probs.")

  return(ps)
}

getParameterSet.Poisson <- function(x, rate, verbose = FALSE){

  if(verbose) message("Parameterised with size and probs.")

  ps <-  ParameterSet$new(id = list("rate"), value = list(1),
                          lower = list(0), upper = list(Inf),
                          class = list("numeric"),
                          settable = list(TRUE),
                          updateFunc = list(NA),
                          description = list("Arrival Rate"))

  return(ps)
}

getParameterSet.Weibull <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <-  ParameterSet$new(id = list("shape","scale"), value = list(1,1),
                          lower = list(0,0), upper = list(Inf,Inf),
                          class = list("numeric","numeric"),
                          settable = list(TRUE,TRUE),
                          updateFunc = list(NA,NA),
                          description = list("Shape paramer", "Scale parameter"))

  return(ps)
}

getParameterSet.StudentT <- function(x, df, verbose = FALSE){

  if(verbose) message("Parameterised with df.")

  ps <- ParameterSet$new(id = list("df"), value = list(1),
                         lower = list(0), upper = list(Inf),
                         class = list("numeric"),
                         settable = list(TRUE),
                         updateFunc = list(NA),
                         description = list("Degrees of Freedom"))

  return(ps)
}

getParameterSet.Pareto <- function(x, shape, scale, verbose = FALSE){

  if(verbose) message("Parameterised with shape and scale.")

  ps <- ParameterSet$new(id = list("shape","scale"), value = list(1, 1),
                         lower = list(0, 0), upper = list(Inf, Inf),
                         class = list("numeric","numeric"),
                         settable = list(TRUE,TRUE),
                         updateFunc = NULL,
                         description = list("Shape parameter","Scale parameter"))

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
                         lower = list(-Inf, 0, 0), upper = list(Inf, Inf, Inf),
                         class = list("numeric","numeric","numeric"),
                         settable = list(TRUE, scale.bool, var.bool),
                         updateFunc = list(NA, NA, "2*self$getParameterValue('scale')^2"),
                         description = list("Mean - Location Parameter",
                                            "Scale - Scale Parameter",
                                            "Variance - Alternate Scale Parameter"))
  return(ps)
}

getParameterSet.Gamma <- function(x, shape, rate, scale = NULL, mean = NULL, verbose = FALSE){

  rate.bool = mean.bool = scale.bool = FALSE

  if(!is.null(rate)){
    if(verbose) message("Parameterised with shape and rate.")
    rate.bool = TRUE
  } else if(!is.null(scale)){
    if(verbose) message("Parameterised with shape and scale.")
    scale.bool = TRUE
  } else{
    if(verbose) message("Parameterised with shape and mean.")
    mean.bool = TRUE
  }

  ps <- ParameterSet$new(id = list("shape","rate","scale","mean"), value = list(1, 1, 1, 1),
                         lower = list(0, 0, 0, 0), upper = list(Inf, Inf, Inf, Inf),
                         class = list("numeric","numeric","numeric","numeric"),
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

getParameterSet.Geometric <- function(x, prob = NULL, qprob = NULL, verbose = FALSE){
  
  prob.bool = qprob.bool = FALSE
  
  if(is.null(prob) & is.null(qprob)){
    if(verbose) message("prob and qprob missing. Parameterised with prob = 0.5.")
    prob.bool = TRUE
  } else if(!is.null(qprob)){
    if(verbose) message("Parameterised with qprob.")
    qprob.bool = TRUE
  } else if(!is.null(prob)){
    if(verbose) message("Parameterised with prob.")
    prob.bool = TRUE
  }
  
  ps <- ParameterSet$new(id = list("prob","qprob"), value = list(0.5, 0.5),
                         lower = list(0, 0), upper = list(1, 1),
                         class = list("numeric","numeric"),
                         settable = list(prob.bool, qprob.bool),
                         updateFunc = list(NULL, "1 - self$getParameterValue('prob')"),
                         description = list("Probability of Success", "Probability of failure"))
  return(ps)
}

getParameterSet.Hypergeometric <- function(x, size = NULL, success = NULL,draws = NULL, verbose = FALSE){
  
  size.bool = success.bool = draws.bool = FALSE
  
  if(is.null(size) & is.null(success) & is.null(draws)){
    if(verbose) message("size, success and draws are missing. Parameterised with size=10,success=5,draws=2.")
    size.bool = TRUE
    success.bool = TRUE
    draws.bool = TRUE
  } else if(!is.null(size)){
    if(verbose) message("Parameterised with size.")
    size.bool = TRUE
  } else if(!is.null(success)){
    if(verbose) message("Parameterised with success.")
    success.bool = TRUE
  }else if(!is.null(draws)){
    if(verbose) message("Parameterised with draws.")
    draws.bool = TRUE
  }
  
  
  ps <- ParameterSet$new(id = list("size","success","draws"), value = list(10, 5,2),
                         lower = list(0, 0,0), upper = list(Inf,Inf,Inf),
                         class = list("numeric","numeric","numeric"),
                         settable = list(size.bool, success.bool,draws.bool),
                         updateFunc = list(NULL,NULL,NULL),
                         description = list("population size", "number of success states in the population","number of draws"))
  
  if(draws<=size & success<=size) return(ps)
  else if(draws>size & success>size) message("the number of success states and the number of draws cannot be larger than population size")
  else if(draws>size) message("the number of draws cannot be larger than population size")
  else if(success>size) message("the number of success states cannot be larger than population size")
    
}



