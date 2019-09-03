#' @include SetInterval_SpecialSet.R ParameterSet.R
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
#' @templateVar omittedVars skewness, kurtosis, entropy, mgf, cf
#' @templateVar constructor samples
#' @templateVar arg1 \code{samples} \tab numeric \tab vector of observed samples. \cr
#' @templateVar constructorDets a vector of elements for the support set.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function.
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
Empirical <- R6::R6Class("Empirical", inherit = SDistribution, lock_objects = F)
Empirical$set("public","name","Empirical")
Empirical$set("public","short_name","Emp")
Empirical$set("public","description","Empirical Probability Distribution.")
Empirical$set("public","package","distr6")

Empirical$set("public","mode",function(which = "all"){
  if(which == "all")
    return(modal(self$support()$elements()))
  else
    return(modal(self$support()$elements())[which])
})
Empirical$set("public","mean",function(){
  return(mean(self$support()$elements()))
})
Empirical$set("public","variance",function(){
  return(var(self$support()$elements()))
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
    return(as.numeric(unlist(private$.data[match(x1, private$.data$samples), "N"]/private$.total)))
  }

  cdf <- function(x1){
    return(as.numeric(unlist(private$.data[findInterval(x1, private$.data$samples), "cumN"]/private$.total)))
  }
  quantile <- function(p){
    p = p * private$.total
    return(as.numeric(unlist(private$.data[findInterval(p, private$.data$cumN, all.inside = TRUE), "samples"])))
  }

  rand <- function(n){
    return(sample(self$support()$elements(), n, TRUE))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Set$new(samples),
                   symmetric = FALSE, type = Reals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})
