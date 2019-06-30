#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gompertz Distribution Documentation
#-------------------------------------------------------------
#' @title Gompertz Distribution
#'
#' @description Mathematical and statistical functions for the Gompertz distribution parameterised
#' with shape and scale. The Gompertz distribution is defined by the pdf,
#' \deqn{f(x) = \alpha\beta exp(x\beta)exp(\alpha)exp(-exp(x\beta)\alpha)}
#' where \eqn{\alpha, \beta > 0} are the shape and scale parameters respectively.
#'
#' @description Mathematical and statistical functions for the Gompertz distribution parameterised
#' with shape and scale.
#'
#' @details Unfortunately the Gompertz distribution is quite complex to deal with and as such no closed
#' form expression exist for its mathematical and statistical properties. Try decorating with
#' \code{\link{CoreStatistics}} for numerical results.
#'
#' @name Gompertz
#'
#' @section Constructor: Gompertz$new(shape = 1, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab positive shape parameter. \cr
#' \code{scale} \tab numeric \tab positive scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Gompertz distribution is parameterised by default with
#' shape = 1 and scale = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- Gompertz$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 1))
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Gompertz Distribution Definition
#-------------------------------------------------------------
Gompertz <- R6::R6Class("Gompertz", inherit = SDistribution, lock_objects = F)
Gompertz$set("public","name","Gompertz")
Gompertz$set("public","short_name","Gomp")
Gompertz$set("public","traits",list(type = PosReals$new(zero = T),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))
Gompertz$set("public","description","Gompertz Probability Distribution.")
Gompertz$set("public","package","distr6")

Gompertz$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Gompertz$set("public","initialize",function(shape = 1, scale = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, verbose)
  suppressMessages(self$setParameterValue(list(shape = shape, scale = scale)))

  pdf <- function(x1){
    return(self$getParameterValue("shape")*self$getParameterValue("scale")*exp(self$getParameterValue("shape"))*
             exp(self$getParameterValue("scale")*x1)*exp(-self$getParameterValue("shape")*exp(self$getParameterValue("scale")*x1)))
  }
  cdf <- function(x1){
    return(1 - exp(-self$getParameterValue("shape")*(exp(self$getParameterValue("scale")*x1)-1)))
  }
  quantile <- function(p){
    return(log(1 + (log(1-p)/-self$getParameterValue("shape")))/self$getParameterValue("scale"))
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }


  suppressMessages(super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T), distrDomain = PosReals$new(zero = T),
                   symmetric  = FALSE))
  invisible(self)
})
