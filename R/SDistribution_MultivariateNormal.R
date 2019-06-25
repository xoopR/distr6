  #' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title MultivariateNormal Distribution
#'
#' @description Mathematical and statistical functions for the MultivariateNormal distribution parameterised
#' with size and probabilites and defined by the pmf,
#' \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' where \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1} are the probabilities for each of the \eqn{K} categories and
#' \eqn{n = 1,2,\ldots} is the number of trials.
#'
#' @details The MultivariateNormal is constructed with a size and probs parameter. Size, number of trials,
#' should not be confused with the \code{K} parameter for number of categories. \code{K} is determined
#' automatically by the number of probabilities supplied to the \code{probs} argument, this also tells the
#' object how many inputs to expect in \code{pdf} and \code{rand}. \code{cdf} and \code{quantile} are omitted
#' as no closed form analytic expression could be found.
#'
#' @name MultivariateNormal
#'
#' @section Constructor: MultivariateNormal$new(size, probs, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab integer \tab number of trials. See details. \cr
#' \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The MultivariateNormal distribution is parameterised by size and prob.
#' Size, N, is given as a single integer greater than zero, such that if \eqn{x} is a vector of \eqn{K} parameters
#' passed to the pmf then it should be true that \eqn{\sum x_i = N}.
#' The length of the probability vector, \eqn{K}, tells the constructor how many arguments to expect
#' to be passed to the pmf function. The probability vector is automatically normalised with
#' \deqn{probs = probs/sum(probs)}.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x=MultivariateNormal$new(means = c(0,0,0), cov = matrix(c(3,-1,-1,-1,1,0,-1,0,1),byrow=TRUE,nrow=3))
#'
#' @export
NULL
#-------------------------------------------------------------
# MultivariateNormal Distribution Definition
#-------------------------------------------------------------
MultivariateNormal <- R6::R6Class("MultivariateNormal", inherit = SDistribution, lock_objects = F)
MultivariateNormal$set("public","name","MultivariateNormal")
MultivariateNormal$set("public","short_name","MultiNorm")
MultivariateNormal$set("public","traits",list(type = Reals$new(dim = "K"),
                                  valueSupport = "continuous",
                                  variateForm = "multivariate"))
MultivariateNormal$set("public","description","Multivariate Normal Probability Distribution.")
MultivariateNormal$set("public","package","distr6")

MultivariateNormal$set("public","mean",function(){
  return(self$getParameterValue("means"))
})
MultivariateNormal$set("public","mode",function(){
  return(self$getParameterValue("means"))
})
MultivariateNormal$set("public","var",function(){
  return(diag(self$cov()))
})
MultivariateNormal$set("public","cov",function(){
  return(self$getParameterValue("cov"))
})
MultivariateNormal$set("public","cor",function(){
  return(self$cov() / (sqrt(self$var() %*% t(self$var()))))
})
MultivariateNormal$set("public","entropy",function(base = 2){
  return(0.5 * log(det(2 * pi * exp(1) * self$getParameterValue("cov")), base))
})
MultivariateNormal$set("public", "mgf", function(t){
  return(exp((t(self$getParameterValue("means")) %*% t) + (0.5 * t(t) * self$getParameterValue("cov") * t)))
})
MultivariateNormal$set("public", "cf", function(t){
  return(exp((1i * t(self$getParameterValue("means")) %*% t) + (0.5 * t(t) * self$getParameterValue("cov") * t)))
})

MultivariateNormal$set("public","setParameterValue",function(lst, error = "warn"){
  if(!is.null(lst$cov)){
    if(any(dim(lst$cov) != c(self$getParameterValue("K"), self$getParameterValue("K"))))
      lst$cov <- matrix(lst$cov, nrow = self$getParameterValue("K"), ncol = self$getParameterValue("K"))
  }
  if(!is.null(lst$prec)){
    if(any(dim(lst$prec) != c(self$getParameterValue("K"), self$getParameterValue("K"))))
      lst$prec <- matrix(lst$prec, nrow = self$getParameterValue("K"), ncol = self$getParameterValue("K"))
  }
  if(!is.null(lst$means)){
    lst$means <- as.numeric(lst$means)
    if(length(lst$means) < self$getParameterValue("K"))
      lst$means <- rep(lst$means, self$getParameterValue("K"))
    if(length(lst$means) > self$getParameterValue("K"))
      lst$means <- lst$means[1:self$getParameterValue("K")]
  }

  return(super$setParameterValue(lst, error))
})
MultivariateNormal$set("public","getParameterValue",function(id, error = "warn"){
  if("cov" %in% id)
    return(matrix(super$getParameterValue("cov", error),nrow = super$getParameterValue("K", error)))
  else
    return(super$getParameterValue(id, error))
})
MultivariateNormal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$means)) lst = c(lst, list(means = paramlst$means))
  if(!is.null(paramlst$cov)) lst = c(lst, list(cov = paramlst$cov))
  if(!is.null(paramlst$prec)) lst = c(lst, list(cov = solve(paramlst$prec)))
  return(lst)
})

MultivariateNormal$set("public","initialize",function(means = rep(0,2), cov = matrix(c(1,1,1,1),nrow=2),
                                                      prec = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, means, cov, prec, verbose)
  self$setParameterValue(list(means = means, cov = cov, prec = prec))

  lst <- rep(list(bquote()), length(means))
  names(lst) <- paste("x",1:length(means),sep="")

  pdf <- function(){

    if(isSymmetric.matrix(self$cov()) & all(eigen(self$cov(),only.values = T)$values > 0)){

      K <- self$getParameterValue("K")
      cov <- self$getParameterValue("cov")
      xs <- matrix(unlist(mget(paste0("x",1:K))), ncol = K)
      means <- matrix(self$getParameterValue("means"), nrow = nrow(xs), ncol = K, byrow = T)

      return(as.numeric((2*pi)^(-K/2) * det(cov)^-0.5 *
               exp(-0.5 * rowSums((xs - means) %*% solve(cov) * (xs - means)))))
    } else
      return(NaN)
  }
  formals(pdf) <- lst

  rand <- function(n){
    ch <- chol(self$cov())
    xs <- matrix(rnorm(self$getParameterValue("K")*n), ncol = n)
    return(data.table::data.table(t(means + ch %*% xs)))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Reals$new(dim = length(means)),
                   distrDomain = Reals$new(dim = length(means)), symmetric = FALSE)
  invisible(self)
})
