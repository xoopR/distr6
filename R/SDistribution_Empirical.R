
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name Empirical
#' @template SDist
#' @templateVar ClassName Empirical
#' @templateVar DistName Empirical
#' @templateVar uses in sampling such as MCMC
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{p(x) = \sum I(x = x_i) / k}
#' @templateVar paramsupport \eqn{x_i \epsilon R, i = 1,...,k}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the support set and uniform probabilities. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar constructor samples
#' @templateVar arg1 \code{samples} \tab numeric \tab vector of observed samples. \cr
#' @templateVar constructorDets a vector of elements for the support set.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function and \code{\link{WeightedDiscrete}} for the closely related WeightedDiscrete distribution.
#'
#' @examples
#' x = Empirical$new(stats::runif(1000)*10)
#'
#' # d/p/q/r
#' x$pdf(1:5)
#' x$cdf(1:5) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Empirical Distribution Definition
#-------------------------------------------------------------
Empirical <- R6Class("Empirical", inherit = SDistribution, lock_objects = F)
Empirical$set("public","name","Empirical")
Empirical$set("public","short_name","Emp")
Empirical$set("public","description","Empirical Probability Distribution.")

Empirical$set("public","mode",function(which = "all"){
  if(which == "all")
    return(modal(unlist(self$support$elements)))
  else
    return(modal(unlist(self$support$elements))[which])
})
Empirical$set("public","mean",function(){
  return(mean(unlist(self$support$elements)))
})
Empirical$set("public","variance",function(){
  return(sum((unlist(self$support$elements) - self$mean())^2)/private$.total)
})
Empirical$set("public","skewness",function(){
  return(sum(((unlist(self$support$elements) - self$mean())/self$stdev())^3)/private$.total)
})
Empirical$set("public","kurtosis",function(excess = TRUE){
  kurt = sum(((unlist(self$support$elements) - self$mean())/self$stdev())^4)/private$.total
  if(excess)
    return(kurt - 3)
  else
    return(kurt)
})
Empirical$set("public","entropy",function(base = 2){
  p = private$.data$N/private$.total
  return(-sum(p * log(p, base)))
})
Empirical$set("public","mgf",function(t){
  if(length(t) == 1)
    return(sum(exp(private$.data$samples*t) * (private$.data$N/private$.total)))
  else{
    nr = length(t)
    nc = length(private$.data$samples)
    return(as.numeric(
      exp(matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = T) *
          matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N/private$.total, nrow = nc, ncol = 1)
    ))
  }
})
Empirical$set("public","cf",function(t){
  if(length(t) == 1)
    return(sum(exp(private$.data$samples*t*1i) * (private$.data$N/private$.total)))
  else{
    nr = length(t)
    nc = length(private$.data$samples)
    return(as.complex(
      exp(matrix(private$.data$samples*1i, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N/private$.total, nrow = nc, ncol = 1)
    ))
  }
})
Empirical$set("public","pgf",function(z){
  if(length(z) == 1)
    return(sum((z^private$.data$samples) * (private$.data$N/private$.total)))
  else{
    nr = length(z)
    nc = length(private$.data$samples)
    return(as.numeric(
      (matrix(z, nrow = nr, ncol = nc) ^ matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = z)) %*%
        matrix(private$.data$N/private$.total, nrow = nc, ncol = 1)
    ))
  }
})

Empirical$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  message("There are no parameters to set.")
  return(NULL)
})

Empirical$set("private",".data",data.table::data.table())
Empirical$set("private",".total", numeric(1))


Empirical$set("public","initialize",function(samples, decorators = NULL, verbose = FALSE){

  samples <- sort(as.numeric(samples))

  private$.data <- data.table::as.data.table(table(samples))
  private$.data$samples <- as.numeric(private$.data$samples)
  private$.data <- cbind(private$.data, cumN = cumsum(private$.data$N))
  private$.total <- length(samples)


  pdf <- function(x1){
    return(as.numeric(unlist(private$.data[match(round(x1, 10), round(private$.data$samples, 10)), "N"]/private$.total)))
  }

  cdf <- function(x1){
    find = findInterval(x1, private$.data$samples)
    find[find == 0] = 1
    return(as.numeric(unlist(private$.data[find, "cumN"]/private$.total)))
  }
  quantile <- function(p){
    p = p * private$.total
    mat = p <= matrix(private$.data$cumN, nrow = length(p), ncol = nrow(private$.data), byrow = T)
    which = apply(mat, 1, function(x) which(x)[1])
    which[is.na(which)] = ncol(mat)
    return(as.numeric(unlist(private$.data[which, "samples"])))
  }

  rand <- function(n){
    return(sample(unlist(self$support$elements), n, TRUE))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Tuple$new(universe = Reals$new(), elements = as.list(samples), class = "numeric"),
                   symmetric = FALSE, type = Reals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Emp", ClassName = "Empirical",
                                                     Type = "\u211D", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "-"))

