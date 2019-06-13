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