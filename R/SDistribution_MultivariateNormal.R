  #' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title MultivariateNormal Distribution
#'
#' @description Mathematical and statistical functions for the MultivariateNormal distribution parameterised
#' with mean and cov or \eqn{prec = cov^{-1}}. The mean-cov parameterisation is defined by the pdf,
#' \deqn{f(x) = (2 * \pi)^{-k/2}det(\Sigma)^{-1/2}exp(-1/2(x-\mu)^T\Sigma^{-1}(x-\mu))}
#' where \eqn{x} is a k-vector to evaluate the distribution at, \eqn{\mu} is the mean k-vector and
#' \eqn{\Sigma} is the covariance matrix; the pdf exists only when \eqn{\Sigma} is positive semi-definite.
#'
#' @details The MultivariateNormal is constructed with a mean and covariance or precision.
#' The parameter \code{K} is automatically updated by counting the length of the mean vector,
#' once constructed this cannot be changed. If a \code{mean} vector of length greater than K is
#' given then this is truncated to the correct length. If a \code{mean} vector of length less than K
#' is given then this replicated and truncated to the correct length. Sampling is performed via the
#' Cholesky decomposition. \code{cdf}, \code{quantile}, \code{skewness} and \code{kurtosis} are omitted
#' as no closed form analytic expression could be found.
#'
#' @name MultivariateNormal
#'
#' @section Constructor: MultivariateNormal$new(mean = rep(0,2), cov = c(1,0,0,1),
#' prec = NULL, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab vector of means. \cr
#' \code{cov} \tab numeric \tab vector or matrix. See details. \cr
#' \code{prec} \tab numeric \tab vector or matrix. See details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The covariance matrix can either be supplied as a matrix or as a
#' vector that can be printed via \code{matrix(cov, nrow = K, byrow = FALSE)}.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Different parameterisations
#' MultivariateNormal$new(mean = c(0,0,0), cov = matrix(c(3,-1,-1,-1,1,0,-1,0,1), byrow=TRUE,nrow=3))
#' MultivariateNormal$new(mean = c(0,0,0), cov = c(3,-1,-1,-1,1,0,-1,0,1)) # Equivalently
#' MultivariateNormal$new(mean = c(0,0,0), prec = c(3,-1,-1,-1,1,0,-1,0,1))
#'
#' x <- MultivariateNormal$new() # Default is bivariate standard normal
#'
#' # Update parameters
#' x$setParameterValue(list(mean = c(1, 2)))
#' x$setParameterValue(list(prec = c(1,0,0,1))) # When any parameter is updated, all others are too!
#' x$parameters()
#'
#' # p/d/q/r
#' # Note the difference from R stats
#' x$pdf(1, 2)
#' # This allows vectorisation:
#' x$pdf(1:3, 2:4)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$var()
#'
#' summary(x)
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
  return(self$getParameterValue("mean"))
})
MultivariateNormal$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})
MultivariateNormal$set("public","var",function(){
  return(self$getParameterValue("cov"))
})
MultivariateNormal$set("public","cor",function(){
  return(self$var() / (sqrt(diag(self$var()) %*% t(diag(self$var())))))
})
MultivariateNormal$set("public","entropy",function(base = 2){
  return(0.5 * log(det(2 * pi * exp(1) * self$getParameterValue("cov")), base))
})
MultivariateNormal$set("public", "mgf", function(t){
  checkmate::assert(length(t) == self$getParameterValue("K"))
  return(exp((self$getParameterValue("mean") %*% t(t(t))) + (0.5 * t %*% self$getParameterValue("cov") %*% t(t(t)))))
})
MultivariateNormal$set("public", "cf", function(t){
  checkmate::assert(length(t) == self$getParameterValue("K"))
  return(exp((1i * self$getParameterValue("mean") %*% t(t(t))) + (0.5 * t %*% self$getParameterValue("cov") %*% t(t(t)))))
})

MultivariateNormal$set("public","setParameterValue",function(lst, error = "warn"){
  if(!is.null(lst$cov)){
    if(any(dim(lst$cov) != c(self$getParameterValue("K"), self$getParameterValue("K"))))
      lst$cov <- matrix(lst$cov, nrow = self$getParameterValue("K"), ncol = self$getParameterValue("K"))
    lst$cov <- as.numeric(lst$cov)
  }
  if(!is.null(lst$prec)){
    if(any(dim(lst$prec) != c(self$getParameterValue("K"), self$getParameterValue("K"))))
      lst$prec <- matrix(lst$prec, nrow = self$getParameterValue("K"), ncol = self$getParameterValue("K"))
    lst$prec <- as.numeric(lst$prec)
  }
  if(!is.null(lst$mean)){
    lst$mean <- as.numeric(lst$mean)
    if(length(lst$mean) < self$getParameterValue("K"))
      lst$mean <- rep(lst$mean, self$getParameterValue("K"))
    if(length(lst$mean) > self$getParameterValue("K"))
      lst$mean <- lst$mean[1:self$getParameterValue("K")]
    lst$mean <- as.numeric(lst$mean)
  }

  return(super$setParameterValue(lst, error))
})
MultivariateNormal$set("public","getParameterValue",function(id, error = "warn"){
  if("cov" %in% id)
    return(matrix(super$getParameterValue("cov", error),nrow = super$getParameterValue("K", error)))
  else if("prec" %in% id)
    return(matrix(super$getParameterValue("prec", error),nrow = super$getParameterValue("K", error)))
  else
    return(super$getParameterValue(id, error))
})
MultivariateNormal$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$cov)) lst = c(lst, list(cov = paramlst$cov))
  if(!is.null(paramlst$prec)) lst = c(lst, list(cov = solve(matrix(paramlst$prec,
                                                                   nrow = self$getParameterValue("K"),
                                                                   ncol = self$getParameterValue("K")))))
  return(lst)
})

MultivariateNormal$set("public","initialize",function(mean = rep(0,2), cov = c(1,0,0,1),
                                                      prec = NULL, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, cov, prec, verbose)
  self$setParameterValue(list(mean = mean, cov = cov, prec = prec))

  lst <- rep(list(bquote()), length(mean))
  names(lst) <- paste("x",1:length(mean),sep="")

  pdf <- function(){

    if(isSymmetric.matrix(self$var()) & all(eigen(self$var(),only.values = T)$values > 0)){

      K <- self$getParameterValue("K")
      cov <- self$getParameterValue("cov")
      xs <- matrix(unlist(mget(paste0("x",1:K))), ncol = K)
      mean <- matrix(self$getParameterValue("mean"), nrow = nrow(xs), ncol = K, byrow = T)

      return(as.numeric((2*pi)^(-K/2) * det(cov)^-0.5 *
               exp(-0.5 * rowSums((xs - mean) %*% solve(cov) * (xs - mean)))))
    } else
      return(NaN)
  }
  formals(pdf) <- lst

  rand <- function(n){
    ch <- chol(self$var())
    xs <- matrix(rnorm(self$getParameterValue("K")*n), ncol = n)
    return(data.table::data.table(t(mean + ch %*% xs)))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Reals$new(dim = length(mean)),
                   distrDomain = Reals$new(dim = length(mean)), symmetric = FALSE)
  invisible(self)
})
