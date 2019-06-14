#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title Multinomial Distribution
#'
#' @description Mathematical and statistical functions for the Multinomial distribution parameterised
#' with size and probabilities.
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
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{cov()} \tab \code{\link{cov}} \cr
#'   \code{cor()} \tab \code{\link{cor}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{pgf(t)} \tab \code{\link{pgf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   }
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

Multinomial$set("public","mean",function(){
  return(self$getParameterValue("size") * self$getParameterValue("probs"))
}) # TEST
Multinomial$set("public","var",function(){
  return(self$getParameterValue("size") * self$getParameterValue("probs") * (1 - self$getParameterValue("probs")))
}) # TEST
Multinomial$set("public","cov",function(){
  cov = self$getParameterValue("probs") %*% t(self$getParameterValue("probs")) * -self$getParameterValue("size")
  diag(cov) = self$var()
  return(cov)
}) # TEST
Multinomial$set("public","cor",function(){
  return(self$cov() / (sqrt(self$var() %*% t(self$var()))))
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
  return((exp(t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
}) # TEST
Multinomial$set("public", "cf", function(t){
  checkmate::assert(length(t) == self$getParameterValue("K"))
  return((exp(1i * t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
}) # TEST
Multinomial$set("public", "pgf", function(z){
  checkmate::assert(length(z) == self$getParameterValue("K"))
  return((self$getParameterValue("probs") * z)^self$getParameterValue("size"))
}) # TEST

Multinomial$set("public","setParameterValue",function(lst, error = "warn"){
  if("probs" %in% names(lst)) lst$probs <- lst$probs/sum(lst$probs)
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

  pdf <- function(x1){
    if(length(x1) != self$getParameterValue("K"))
      stop(paste("x1 should be of length",self$getParameterValue("K")))


    if(sum(x1) != self$getParameterValue("size"))
      return(0)

    return(dmultinom(x1, self$getParameterValue("size"), self$getParameterValue("probs")))
  }
  rand <- function(n){
    rmultinom(n, self$getParameterValue("size"), self$getParameterValue("probs"))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Set$new(0:size, dim = length(probs)),
                   distrDomain = PosIntegers$new(zero = T, dim = length(probs)), symmetric = FALSE)
  invisible(self)
})
