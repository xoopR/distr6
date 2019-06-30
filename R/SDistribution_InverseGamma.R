#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Inverse Gamma Distribution Documentation
#-------------------------------------------------------------
#' @title Inverse Gamma Distribution
#'
#' @description Mathematical and statistical functions for the InverseGamma distribution parameterised
#' with shape and scale and defined by the pdf
#' \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha) * x^(-\alpha-1) * exp(-\beta/x)}
#' where \eqn{\alpha > 0} is the shape parameter, \eqn{\beta > 0} is the scale parameter and
#' \eqn{\Gamma} is the gamma function.
#'
#' @details The InverseGamma Distribution uses the extraDistr package as it provides a quantile function
#' that computes the inverse upper incomplete Gamma function. The BesselK function from the package
#' Bessel is used to compute the characteristic function.
#'
#' @name InverseGamma
#'
#' @section Constructor: InverseGamma$new(shape = 1, scale = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{scale} \tab numeric \tab scale parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The InverseGamma Distribution is parameterised by default with shape = 1 and scale = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x  = InverseGamma$new(shape = 1, scale = 4)
#'
#' # Update parameters
#' x$setParameterValue(list(scale = 2)) # When any parameter is updated, all others are too!
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
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
# InverseGamma Distribution Definition
#-------------------------------------------------------------
InverseGamma <- R6::R6Class("InverseGamma", inherit = SDistribution, lock_objects = F)
InverseGamma$set("public","name","InverseGamma")
InverseGamma$set("public","short_name","invgam")

InverseGamma$set("public","traits",list(type = PosReals$new(),
                                       valueSupport = "continuous",
                                       variateForm = "univariate"))

InverseGamma$set("public","description","Inverse Gamma Probability Distribution.")
InverseGamma$set("public","package","extraDistr")

InverseGamma$set("public","mean",function(){
  if(self$getParameterValue("shape") > 1)
    return(self$getParameterValue("scale")/(self$getParameterValue("shape")-1))
  else
    return(NaN)
})
InverseGamma$set("public","var",function(){
  if(self$getParameterValue("shape") > 2)
    return(self$getParameterValue("scale")^2/((self$getParameterValue("shape")-1)^2 * (self$getParameterValue("shape")-2)))
  else
    return(NaN)
})
InverseGamma$set("public","mode",function(){
  return(self$getParameterValue("scale")/(self$getParameterValue("shape")+1))
})
InverseGamma$set("public","skewness",function() {
  if(self$getParameterValue("shape") > 3)
    return((4 * sqrt(self$getParameterValue("shape")-2))/(self$getParameterValue("shape")-3))
  else
    return(NaN)
})
InverseGamma$set("public","kurtosis",function(excess = TRUE){
  if(self$getParameterValue("shape") > 4){
    kur = (6*(5*self$getParameterValue("shape") - 11))/
      ((self$getParameterValue("shape")-3) * (self$getParameterValue("shape")-4))
    if(excess)
      return(kur)
    else
      return(kur + 3)
  } else
    return(NaN)
})
InverseGamma$set("public","entropy",function(base = 2){
  return(self$getParameterValue("shape") +
    log(self$getParameterValue("scale") * gamma(self$getParameterValue("shape")), base) -
    (1 + self$getParameterValue("shape")) * digamma(self$getParameterValue("shape")))
})
InverseGamma$set("public", "mgf", function(t){
  return(NaN)
})
InverseGamma$set("public", "cf", function(t){
  p1 = (2 * (-1i * self$getParameterValue("scale") *t)^(self$getParameterValue("shape")/2))/gamma(self$getParameterValue("shape"))
  p2 = Bessel::BesselK(sqrt(-4i*self$getParameterValue("scale")*t), self$getParameterValue("shape"))
  return(p1*p2)
})

InverseGamma$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

InverseGamma$set("public","initialize",function(shape = 1,scale = 1, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.InverseGamma(self, shape, scale, verbose)
  self$setParameterValue(list(shape=shape,scale = scale))

  pdf <- function(x1) extraDistr::dinvgamma(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  cdf <- function(x1) extraDistr::pinvgamma(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  quantile <- function(p) extraDistr::qinvgamma(p, self$getParameterValue("shape"), self$getParameterValue("scale"))
  rand <- function(n) extraDistr::rinvgamma(n, self$getParameterValue("shape"), self$getParameterValue("scale"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(), distrDomain = PosReals$new(),
                   symmetric  = FALSE)
  invisible(self)
})

