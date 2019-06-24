#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title Categorical Distribution
#'
#' @description Mathematical and statistical functions for the Categorical distribution parameterised
#' with size and probabilites and defined by the pmf,
#' \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' where \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1} are the probabilities for each of the \eqn{K} categories and
#' \eqn{n = 1,2,\ldots} is the number of trials.
#'
#' @details The Categorical is constructed with a size and probs parameter. Size, number of trials,
#' should not be confused with the \code{K} parameter for number of categories. \code{K} is determined
#' automatically by the number of probabilities supplied to the \code{probs} argument, this also tells the
#' object how many inputs to expect in \code{pdf} and \code{rand}. \code{cdf} and \code{quantile} are omitted
#' as no closed form analytic expression could be found.
#'
#' @name Categorical
#'
#' @section Constructor: Categorical$new(size, probs, decorators = NULL, verbose = FALSE)
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
#' @section Constructor Details: The Categorical distribution is parameterised by size and prob.
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
#' x = Categorical$new("Bapple","Banana",2,probs=c(0.2,0.4,1))
#' x$pdf(c("Bapple", "Carot", 1, 2))
#' x$rand(10)
#'
#' @export
NULL
#-------------------------------------------------------------
# Categorical Distribution Definition
#-------------------------------------------------------------
Categorical <- R6::R6Class("Categorical", inherit = SDistribution, lock_objects = F)
Categorical$set("public","name","Categorical")
Categorical$set("public","short_name","Cat")
Categorical$set("public","traits",list(type = Complex$new(),
                                  valueSupport = "discrete",
                                  variateForm = "univariate"))
Categorical$set("public","description","Categorical Probability Distribution.")
Categorical$set("public","package","distr6")

Categorical$set("public","mode",function(){
  return(self$support()$elements()[which.max(self$getParameterValue("probs"))])
})

Categorical$set("public","setParameterValue",function(lst, error = "warn"){
  if("probs" %in% names(lst)) lst$probs <- lst$probs/sum(lst$probs)
  checkmate::assert(length(lst$probs) == self$getParameterValue("categories"))
  super$setParameterValue(lst, error)
})

Categorical$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$probs)) lst = c(lst, list(probs = paramlst$probs))
  return(lst)
})

Categorical$set("public","initialize",function(..., probs, decorators = NULL, verbose = FALSE){

  checkmate::assert(length(list(...)) == length(probs))

  private$.parameters <- getParameterSet(self, probs, verbose)
  self$setParameterValue(list(probs = probs))

  pdf <- function(x1){
    return(self$getParameterValue("probs")[self$support()$elements() %in% x1])
  }

  rand <- function(n){
    return(sample(self$support()$elements(), n, TRUE, self$getParameterValue("probs")))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Set$new(...),
                   distrDomain = PosIntegers$new(zero = T, dim = length(probs)), symmetric = FALSE)
  invisible(self)
})
