#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Inverse Gamma Distribution Documentation
#-------------------------------------------------------------
#' @name InverseGamma
#' @template SDist
#' @templateVar ClassName InverseGamma
#' @templateVar DistName Inverse Gamma
#' @templateVar uses in Bayesian statistics as the posterior distribution from the unknown variance in a Normal distribution
#' @templateVar params shape, \eqn{\alpha}, and scale, \eqn{\beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\beta^\alpha)/\Gamma(\alpha)x^{-\alpha-1}exp(-\beta/x)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{\Gamma} is the gamma function
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{cf}
#' @templateVar additionalDetails The distribution is implemented by interfacing the \code{extraDistr} package.
#' @templateVar constructor shape = 1, scale = 1
#' @templateVar arg1 \code{shape} \tab numeric \tab shape parameter. \cr
#' @templateVar arg2 \code{scale} \tab numeric \tab scale parameter. \cr
#' @templateVar constructorDets \code{shape} and \code{scale} as positive numerics.
#' @templateVar additionalSeeAlso \code{\link[extraDistr]{InvGamma}} for the d/p/q/r implementation.
#'
#' @examples
#' x  = InverseGamma$new(shape = 1, scale = 4)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(scale = 2)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
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
# InverseGamma Distribution Definition
#-------------------------------------------------------------
InverseGamma <- R6::R6Class("InverseGamma", inherit = SDistribution, lock_objects = F)
InverseGamma$set("public","name","InverseGamma")
InverseGamma$set("public","short_name","invgam")
InverseGamma$set("public","description","Inverse Gamma Probability Distribution.")
InverseGamma$set("public","package","extraDistr")

InverseGamma$set("public","mean",function(){
  if(self$getParameterValue("shape") > 1)
    return(self$getParameterValue("scale")/(self$getParameterValue("shape")-1))
  else
    return(NaN)
})
InverseGamma$set("public","variance",function(){
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
InverseGamma$set("public", "pgf", function(z){
  return(NaN)
})
# InverseGamma$set("public", "cf", function(t){
#   p1 = (2 * (-1i * self$getParameterValue("scale") *t)^(self$getParameterValue("shape")/2))/gamma(self$getParameterValue("shape"))
#   p2 = Bessel::BesselK(sqrt(-4i*self$getParameterValue("scale")*t), self$getParameterValue("shape"))
#   return(p1*p2)
# })

InverseGamma$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape= paramlst$shape))
  if(!is.null(paramlst$scale)) lst = c(lst, list(scale = paramlst$scale))
  return(lst)
})

InverseGamma$set("public","initialize",function(shape = 1,scale = 1, decorators = NULL,
                                         verbose = FALSE){

  private$.parameters <- getParameterSet.InverseGamma(self, shape, scale, verbose)
  self$setParameterValue(shape=shape, scale = scale)

  pdf <- function(x1) extraDistr::dinvgamma(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  cdf <- function(x1) extraDistr::pinvgamma(x1, self$getParameterValue("shape"), self$getParameterValue("scale"))
  quantile <- function(p) extraDistr::qinvgamma(p, self$getParameterValue("shape"), self$getParameterValue("scale"))
  rand <- function(n) extraDistr::rinvgamma(n, self$getParameterValue("shape"), self$getParameterValue("scale"))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = PosReals$new(),
                   symmetric  = FALSE,type = PosReals$new(),
                   valueSupport = "continuous",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "invgam", ClassName = "InverseGamma",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "extraDistr"))
