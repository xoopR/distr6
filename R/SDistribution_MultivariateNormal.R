  #' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name MultivariateNormal
#' @template SDist
#' @templateVar ClassName MultivariateNormal
#' @templateVar DistName Multivariate Normal
#' @templateVar uses to generalise the Normal distribution to higher dimensions, and is commonly associated with Gaussian Processes
#' @templateVar params mean, \eqn{\mu}, and covariance matrix, \eqn{\Sigma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x_1,...,x_k) = (2 * \pi)^{-k/2}det(\Sigma)^{-1/2}exp(-1/2(x-\mu)^T\Sigma^{-1}(x-\mu))}
#' @templateVar paramsupport \eqn{\mu \epsilon R^{k}} and \eqn{\Sigma \epsilon R^{k x k}}
#' @templateVar distsupport the Reals and only when the covariance matrix is positive-definite
#' @templateVar omittedVars \code{skewness} and \code{kurtosis}
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
#' @templateVar additionalDetails The parameter \code{K} is automatically updated by counting the length of the mean vector and once constructed this cannot be changed. If a \code{mean} vector of length greater than K is given then this is truncated to the correct length. If a \code{mean} vector of length less than K is given then this replicated and truncated to the correct length. Similarly \code{cov} and \code{prec} are internally coerced with \code{matrix(cov, nrow = K, byrow = FALSE)}. \cr\cr Sampling is performed via the Cholesky decomposition using \code{\link[base]{chol}}.
#' @templateVar constructor mean = rep(0,2), cov = c(1,0,0,1), prec = NULL
#' @templateVar arg1 \code{mean} \tab numeric \tab vector of means. \cr
#' @templateVar arg2 \code{cov} \tab numeric \tab vector or matrix. See details. \cr
#' @templateVar arg3 \code{prec} \tab numeric \tab vector or matrix. See details. \cr
#' @templateVar constructorDets \code{mean} as a vector of numerics and either \code{cov} or \code{prec} as positive semi-definite matrices. These are related via, \deqn{prec = cov^{-1}} If \code{prec} is given then \code{cov} is ignored. \cr\cr The covariance matrix can either be supplied as a matrix or as a vector that can be coerced via \code{matrix(cov, nrow = K, byrow = FALSE)}.
#' @templateVar additionalSeeAlso \code{\link[base]{chol}} for the implementation of the Cholesky decomposition. \code{\link{Normal}} for a special case of the Multivariate Normal distribution.
#' @templateVar additionalReferences  Gentle, J.E. (2009). Computational Statistics. Statistics and Computing. New York: Springer. pp. 315–316. doi:10.1007/978-0-387-98144-4. ISBN 978-0-387-98143-7.
#'
#' @examples
#' # Different parameterisations
#' MultivariateNormal$new(mean = c(0,0,0), cov = matrix(c(3,-1,-1,-1,1,0,-1,0,1), byrow=TRUE,nrow=3))
#' MultivariateNormal$new(mean = c(0,0,0), cov = c(3,-1,-1,-1,1,0,-1,0,1)) # Equivalently
#' MultivariateNormal$new(mean = c(0,0,0), prec = c(3,-1,-1,-1,1,0,-1,0,1))
#'
#' # Default is bivariate standard normal
#' x <- MultivariateNormal$new()
#'
#' # Update parameters
#' x$setParameterValue(mean = c(1, 2))
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(prec = c(1,0,0,1))
#' x$parameters()
#'
#' # d/p/q/r
#' # Note the difference from R stats
#' x$pdf(1, 2)
#' # This allows vectorisation:
#' x$pdf(1:3, 2:4)
#' x$rand(4)
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
# MultivariateNormal Distribution Definition
#-------------------------------------------------------------
MultivariateNormal <- R6::R6Class("MultivariateNormal", inherit = SDistribution, lock_objects = F)
MultivariateNormal$set("public","name","MultivariateNormal")
MultivariateNormal$set("public","short_name","MultiNorm")
MultivariateNormal$set("public","description","Multivariate Normal Probability Distribution.")
MultivariateNormal$set("public","package","distr6")

MultivariateNormal$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
MultivariateNormal$set("public","mode",function(){
  return(self$getParameterValue("mean"))
})
MultivariateNormal$set("public","variance",function(){
  return(self$getParameterValue("cov"))
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
MultivariateNormal$set("public", "pgf", function(z){
  return(NaN)
})


MultivariateNormal$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if(!is.null(lst$cov)){
    if(any(dim(lst$cov) != c(self$getParameterValue("K"), self$getParameterValue("K"))))
      lst$cov <- matrix(lst$cov, nrow = self$getParameterValue("K"), ncol = self$getParameterValue("K"))
    lst$cov <- as.numeric(lst$cov)
  }
  if(!is.null(lst$prec)){
    if(any(dim(lst$prec) != c(self$getParameterValue("K"), self$getParameterValue("K"))) |
       length(dim) != self$getParameterValue("K")^2)
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

  super$setParameterValue(lst = lst, error = error)
  invisible(self)
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
  self$setParameterValue(mean = mean, cov = cov, prec = prec)

  lst <- rep(list(bquote()), length(mean))
  names(lst) <- paste("x",1:length(mean),sep="")

  pdf <- function(){

    if(isSymmetric.matrix(self$variance()) & all(eigen(self$variance(),only.values = T)$values > 0)){

      K <- self$getParameterValue("K")
      call <- mget(paste0("x",1:K))

      if(!all(unlist(lapply(call, is.numeric))))
        stop(paste(K,"arguments expected."))

      if(length(unique(unlist(lapply(call,length)))) > 1)
        stop("The same number of points must be passed to each variable.")

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
    ch <- chol(self$variance())
    xs <- matrix(rnorm(self$getParameterValue("K")*n), ncol = n)
    return(data.table::data.table(t(mean + ch %*% xs)))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Reals$new(dim = length(mean)),
                   symmetric = FALSE,type = Reals$new(dim = "K"),
                   valueSupport = "continuous",
                   variateForm = "multivariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "MultiNorm", ClassName = "MultivariateNormal",
                                                     Type = "\u211D^K", ValueSupport = "continuous",
                                                     VariateForm = "multivariate",
                                                     Package = "distr6"))
