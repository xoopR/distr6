#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name Categorical
#' @template SDist
#' @templateVar ClassName Categorical
#' @templateVar DistName Categorical
#' @templateVar uses in classification supervised learning
#' @templateVar params a given support set, \eqn{x_1,...,x_k}, and respective probabilities, \eqn{p_1,...,p_k},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Only the mode, pdf, cdf, quantile and rand are available for this Distribution, all other methods return \code{NaN}. Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the support set and the probabilities from the \code{probs} parameter. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar constructor ..., probs
#' @templateVar arg1 \code{...} \tab ANY \tab elements in the support Set. See details. \cr
#' @templateVar arg2 \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' @templateVar constructorDets a series of elements for the support set and \code{probs} determining the probability of each category occurring. The length of the probability list should equal the number of elements. The probability vector is automatically normalised with \deqn{probs = probs/sum(probs)} If no arguments are given, then defaults to one element '1' with probability one.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function.
#'
#' @examples
#' # Note probabilities are automatically normalised
#' x = Categorical$new("Bapple","Banana",2,probs=c(0.2,0.4,1))
#'
#' # Only the probabilities can be changed and must the same length as in construction
#' x$setParameterValue(probs = c(0.1,0.2,0.7))
#'
#' # d/p/q/r
#' x$pdf(c("Bapple", "Carrot", 1, 2))
#' x$cdf("Banana") # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mode()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Categorical Distribution Definition
#-------------------------------------------------------------
Categorical <- R6::R6Class("Categorical", inherit = SDistribution, lock_objects = F)
Categorical$set("public","name","Categorical")
Categorical$set("public","short_name","Cat")
Categorical$set("public","description","Categorical Probability Distribution.")
Categorical$set("public","package","distr6")

Categorical$set("public","mode",function(which = "all"){
  if(which == "all")
    return(self$support()$elements()[self$getParameterValue("probs")==max(self$getParameterValue("probs"))])
  else
    return(self$support()$elements()[self$getParameterValue("probs")==max(self$getParameterValue("probs"))][which])
})
Categorical$set("public","mean",function(){
  return(NaN)
})
Categorical$set("public","variance",function(){
  return(NaN)
})
Categorical$set("public","skewness",function(){
  return(NaN)
})
Categorical$set("public","kurtosis",function(){
  return(NaN)
})
Categorical$set("public","entropy",function(){
  return(NaN)
})
Categorical$set("public","mgf",function(t){
  return(NaN)
})
Categorical$set("public","pgf",function(t){
  return(NaN)
})
Categorical$set("public","cf",function(t){
  return(NaN)
})

Categorical$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("probs" %in% names(lst)) lst$probs <- lst$probs/sum(lst$probs)
  checkmate::assert(length(lst$probs) == self$getParameterValue("categories"))
  super$setParameterValue(lst = lst, error = error)
  invisible(self)
})

Categorical$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$probs)) lst = c(lst, list(probs = paramlst$probs))
  return(lst)
})

Categorical$set("public","initialize",function(..., probs, decorators = NULL, verbose = FALSE){

  dots = list(...)

  if(length(dots)==0){
    probs = 1
    dots = 1
    support = Set$new(1)
  } else
    support = Set$new(...)

  checkmate::assert(length(dots) == length(probs))

  private$.parameters <- getParameterSet(self, probs, verbose)
  self$setParameterValue(probs = probs)

  pdf <- function(x1){
    return(self$getParameterValue("probs")[self$support()$elements() %in% x1])
  }
  cdf <- function(x1){
    return(cumsum(self$pdf(self$support()$elements()))[self$support()$elements() %in% x1])
  }
  quantile <- function(p){
    cdf = matrix(self$cdf(self$support()$elements()), ncol = length(self$support()$elements()), nrow = length(p), byrow = T)
    return(self$support()$elements()[apply(cdf >= p, 1, function(x) min(which(x)))])
  }
  rand <- function(n){
    return(sample(self$support()$elements(), n, TRUE, self$getParameterValue("probs")))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = support,
                   symmetric = FALSE, type = Complex$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Cat", ClassName = "Categorical",
                                                     Type = "\u2102", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))
