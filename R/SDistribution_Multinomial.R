#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title Multinomial Distribution
#'
#' @description Mathematical and statistical functions for the Multinomial distribution parameterised
#' with size and probabilities and defined by the pmf,
#' \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' where \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1} are the probabilities for each of the \eqn{K} categories and
#' \eqn{n = 1,2,\ldots} is the number of trials. The distribution is supported on \eqn{\sum x_i = N}.
#'
#' @details The multinomial is constructed with a size and probs parameter. Size, number of trials,
#' should not be confused with the \code{K} parameter for number of categories. \code{K} is determined
#' automatically by the number of probabilities supplied to the \code{probs} argument, this also tells the
#' object how many inputs to expect in \code{pdf} and \code{rand}. \code{cdf} and \code{quantile} are omitted
#' as no closed form analytic expression could be found.
#'
#' @name Multinomial
#'
#' @section Constructor: Multinomial$new(size, probs, decorators = NULL, verbose = FALSE)
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
#' @section Constructor Details: The Multinomial distribution is parameterised by size and prob.
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
#' x <- Multinomial$new(size = 5, probs = c(0.1, 0.5, 0.9)) # Automatically normalised
#'
#' # Update parameters
#' x$setParameterValue(list(size = 10))
#' # Number of categories cannot be changed after construction
#' x$setParameterValue(list(probs = c(1,2,3)))
#' x$parameters()
#'
#' # p/d/q/r
#' # Note the difference from R stats
#' x$pdf(4, 4, 2)
#' # This allows vectorisation:
#' x$pdf(c(1,4),c(2,4),c(7,2))
#'
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
# Multinomial Distribution Definition
#-------------------------------------------------------------
Multinomial <- R6::R6Class("Multinomial", inherit = SDistribution, lock_objects = F)
Multinomial$set("public","name","Multinomial")
Multinomial$set("public","short_name","Multinom")
Multinomial$set("public","traits",list(type = PosIntegers$new(zero = T, dim = "K"),
                                  valueSupport = "discrete",
                                  variateForm = "multivariate"))
Multinomial$set("public","description","Multinomial Probability Distribution.")
Multinomial$set("public","package","stats")

Multinomial$set("public","mean",function(){
  return(self$getParameterValue("size") * self$getParameterValue("probs"))
}) # TEST
Multinomial$set("public","var",function(){
  cov = self$getParameterValue("probs") %*% t(self$getParameterValue("probs")) * -self$getParameterValue("size")
  diag(cov) = self$getParameterValue("size") * self$getParameterValue("probs") * (1 - self$getParameterValue("probs"))
  return(cov)
})
Multinomial$set("public","cor",function(){
  return(self$var() / (sqrt(diag(self$var()) %*% t(diag(self$var())))))
}) # TEST
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

Multinomial$set("public","setParameterValue",function(lst, error = "warn"){
  if("probs" %in% names(lst)){
    checkmate::assert(length(lst$probs) == self$getParameterValue("K"),
                      .var.name = "Number of categories cannot be changed after construction.")
    lst$probs <- lst$probs/sum(lst$probs)
    }
  super$setParameterValue(lst, error)
})

Multinomial$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
  if(!is.null(paramlst$probs)) lst = c(lst, list(probs = paramlst$probs))
  return(lst)
})

Multinomial$set("public","initialize",function(size, probs, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, size, probs, verbose)
  self$setParameterValue(list(size = size, probs = probs))

  lst <- rep(list(bquote()), length(probs))
  names(lst) <- paste("x",1:length(probs),sep="")

  pdf <- function(){

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
                   distrDomain = PosIntegers$new(zero = T, dim = length(probs)), symmetric = FALSE)
  invisible(self)
})
