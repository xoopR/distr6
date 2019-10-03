#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name Multinomial
#' @template SDist
#' @templateVar ClassName Multinomial
#' @templateVar DistName Multinomial
#' @templateVar uses to extend the binomial distribution to multiple variables, for example to model the rolls of multiple dice multiple times
#' @templateVar params number of trials, \eqn{n}, and probabilities of success, \eqn{p_1,...,p_k},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' @templateVar paramsupport \eqn{p_i, i = {1,\ldots,k}; \sum p_i = 1} and \eqn{n = {1,2,\ldots}}
#' @templateVar distsupport \eqn{\sum x_i = N}
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
#' @templateVar constructor size = 10, probs = c(0.5, 0.5)
#' @templateVar arg1 \code{size} \tab numeric \tab number of trials. See details. \cr
#' @templateVar arg2 \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' @templateVar constructorDets \code{size} as a positive whole number and \code{probs} as a vector of numerics between 0 and 1. The length of the probability vector, \eqn{K}, tells the constructor how many arguments to expect to be passed to the maths/stats methods. The probability vector is automatically normalised with \deqn{probs = probs/sum(probs)}.
#' @templateVar additionalSeeAlso \code{\link{Binomial}} for a special case of the Multinomial distribution.
#'
#' @examples
#' x <- Multinomial$new(size = 5, probs = c(0.1, 0.5, 0.9)) # Automatically normalised
#'
#' # Update parameters
#' x$setParameterValue(size = 10)
#' # Number of categories cannot be changed after construction
#' x$setParameterValue(probs = c(1,2,3))
#' x$parameters()
#'
#' # d/p/q/r
#' # Note the difference from R stats
#' x$pdf(4, 4, 2)
#' # This allows vectorisation:
#' x$pdf(c(1,4),c(2,4),c(7,2))
#'
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
# Multinomial Distribution Definition
#-------------------------------------------------------------
Multinomial <- R6::R6Class("Multinomial", inherit = SDistribution, lock_objects = F)
Multinomial$set("public","name","Multinomial")
Multinomial$set("public","short_name","Multinom")
Multinomial$set("public","description","Multinomial Probability Distribution.")
Multinomial$set("public","package","stats")

Multinomial$set("public","mean",function(){
  return(self$getParameterValue("size") * self$getParameterValue("probs"))
}) # TEST
Multinomial$set("public","variance",function(){
  cov = self$getParameterValue("probs") %*% t(self$getParameterValue("probs")) * -self$getParameterValue("size")
  diag(cov) = self$getParameterValue("size") * self$getParameterValue("probs") * (1 - self$getParameterValue("probs"))
  return(cov)
})
Multinomial$set("public","skewness",function(){
  return(NaN)
})
Multinomial$set("public","kurtosis",function(excess = TRUE){
  return(NaN)
})
Multinomial$set("public","entropy",function(base = 2){
  size = self$getParameterValue("size")
  probs = self$getParameterValue("probs")
  K = self$getParameterValue("K")

  s1 = -log(factorial(size), base)
  s2 = -size * sum(probs * log(probs, base))
  s3 = 0
  for(i in 1:K){
    for(j in 0:size){
      s3 = s3 + (choose(size, j) * (probs[[i]]^j) * ((1-probs[[i]])^(size-j)) * (log(factorial(j), base)))
    }
  }

  return(s1 + s2 + s3)
}) # TEST
Multinomial$set("public", "mgf", function(t){
  checkmate::assert(length(t) == self$getParameterValue("K"))
  return(sum(exp(t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
}) # TEST
Multinomial$set("public", "cf", function(t){
  checkmate::assert(length(t) == self$getParameterValue("K"))
  return(sum(exp(1i * t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
}) # TEST
Multinomial$set("public", "pgf", function(z){
  checkmate::assert(length(z) == self$getParameterValue("K"))
  return(sum(self$getParameterValue("probs") * z)^self$getParameterValue("size"))
}) # TEST

Multinomial$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("probs" %in% names(lst)){
    checkmate::assert(length(lst$probs) == self$getParameterValue("K"),
                      .var.name = "Number of categories cannot be changed after construction.")
    lst$probs <- lst$probs/sum(lst$probs)
    }
  super$setParameterValue(lst = lst, error = error)
  invisible(self)
})

Multinomial$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
  if(!is.null(paramlst$probs)) lst = c(lst, list(probs = paramlst$probs))
  return(lst)
})

Multinomial$set("public","initialize",function(size = 10, probs = c(0.5, 0.5), decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, size, probs, verbose)
  self$setParameterValue(size = size, probs = probs)

  lst <- rep(list(bquote()), length(probs))
  names(lst) <- paste("x",1:length(probs),sep="")

  pdf <- function(){

    call = mget(paste0("x",1:self$getParameterValue("K")))

    if(!all(unlist(lapply(call, is.numeric))))
      stop(paste(self$getParameterValue("K"),"arguments expected."))

    if(length(unique(unlist(lapply(call,length)))) > 1)
      stop("The same number of points must be passed to each variable.")

    x = do.call(cbind,mget(paste0("x",1:self$getParameterValue("K"))))
    z = apply(x, 1, function(y){
      if(sum(y) != self$getParameterValue("size"))
        return(0)
      else
        return(dmultinom(y, self$getParameterValue("size"), self$getParameterValue("probs")))
    })

    return(z)

  }
  formals(pdf) <- lst

  rand <- function(n){
    return(data.table::data.table(t(rmultinom(n, self$getParameterValue("size"), self$getParameterValue("probs")))))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Set$new(0:size, dim = length(probs)),
                   symmetric = FALSE, type = Naturals$new(dim = "K"),
                   valueSupport = "discrete",
                   variateForm = "multivariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Multinom", ClassName = "Multinomial",
                                                     Type = "\u21150^K", ValueSupport = "discrete",
                                                     VariateForm = "multivariate",
                                                     Package = "stats"))

