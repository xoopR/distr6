#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Gompertz Distribution Documentation
#-------------------------------------------------------------
#' @name Gompertz
#' @template SDist
#' @templateVar ClassName Gompertz
#' @templateVar DistName Gompertz
#' @templateVar uses in survival analysis particularly to model adult mortality rates.
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \alpha\beta exp(x\beta)exp(\alpha)exp(-exp(x\beta)\alpha)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}
#' @templateVar distsupport the Non-Negative Reals
#' @templateVar omittedVars \code{mean}, \code{var}, \code{mgf}, \code{cf}, \code{entropy}, \code{skewness} and \code{kurtosis}
#' @templateVar additionalDetails Unfortunately the Gompertz distribution is quite complex to deal with and as such no closed form expressions exist for its mathematical and statistical properties.
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab positive shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab positive scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#'
#' @examples
#' x <- Gompertz$new(shape = 2, scale = 3)
#'
#' # Update parameters
#' x$setParameterValue(scale = 1)
#' x$parameters()
#'
#' # d/p/q/r
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
Gompertz$set("public","description","Gompertz Probability Distribution.")
Gompertz$set("public","package","distr6")

Gompertz$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

Gompertz$set("public", "pgf", function(z){
  return(NaN)
})

Gompertz$set("public","initialize",function(shape = 1, scale = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, shape, scale, verbose)
  suppressMessages(self$setParameterValue(shape = shape, scale = scale))

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


  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(zero = T),
                   symmetric  = FALSE,type = PosReals$new(zero = T),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Gomp", ClassName = "Gompertz",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))

