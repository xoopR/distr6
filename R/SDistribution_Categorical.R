#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title Categorical Distribution
#'
#' @description Mathematical and statistical functions for the Categorical distribution parameterised
#' with a given support Set, probabilities and defined by the pmf,
#' \deqn{f(X_1 = x_i) = p_i}
#' where \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1} are the probabilities for each of the \eqn{x_1,...,x_n}
#' elements in the support set.
#'
#' @details Only the mode, pdf, cdf and rand are available for this Distribution. Sampling from this
#' distribution is performed with the \code{\link[base]{sample}} function with the elements given as
#' the support set and the probabilities from the \code{probs} parameter. The cdf assumes that the elements
#' are supplied in an indexed order (otherwise the results are meaningless).
#'
#' @name Categorical
#'
#' @section Constructor: Categorical$new(..., probs, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{...} \tab ANY \tab elements in the support Set. See details. \cr
#' \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Categorical distribution is constructed with a series of elements for the support set
#' and a probs parameter determining the probability of each category occurring. The length of the probability
#' list should equal the number of elements. The probability vector is automatically normalised with
#' \deqn{probs = probs/sum(probs)}
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#'
#' @examples
#' # Note probabilities are automatically normalised
#' x = Categorical$new("Bapple","Banana",2,probs=c(0.2,0.4,1))
#'
#' # Only the probabilities can be changed and must the same length as in construction
#' x$setParameterValue(list(probs = c(0.1,0.2,0.7)))
#'
#' # p/d/q/r
#' x$pdf(c("Bapple", "Carrot", 1, 2))
#' x$cdf("Banana") # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mode()
#'
#' summary(x)
#'
#' @examples
#'
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

  cdf <- function(x1){
    if(length(x1) > 1)
      cdfs = sapply(x1, function(x) sum(self$pdf(self$support()$elements()[1:which(self$support()$elements() %in% x)])))
    else
      cdfs = sum(self$pdf(self$support()$elements()[1:which(self$support()$elements() %in% x1)]))
    return(cdfs)
  }

  rand <- function(n){
    return(sample(self$support()$elements(), n, TRUE, self$getParameterValue("probs")))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, rand = rand,
                   support = Set$new(...),
                   distrDomain = PosIntegers$new(zero = T, dim = length(probs)), symmetric = FALSE)
  invisible(self)
})
