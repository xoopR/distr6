#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Wald Distribution Documentation
#-------------------------------------------------------------
#' @title Wald Distribution
#'
#' @description Mathematical and statistical functions for the Wald (Inverse Gaussian) distribution parameterised
#' with mean and shape and defined by the pdf,
#' \deqn{f(x) = (\lambda/2\pi x^3)^0.5 exp((-\lambda(x-\mu)^2)/(2\mu^2x))}
#' where \eqn{\lambda > 0} is the shape parameter and \eqn{\mu > 0} is the mean parameter.
#'
#' @name Wald
#'
#' @section Constructor: Wald$new(mean = 1, shape = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{mean} \tab numeric \tab location parameter. \cr
#' \code{shape} \tab numeric \tab shape parameter. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Wald distribution is parameterised with
#' mean and shape. Default parameterisation is with mean = 1 and shape = 1.
#'
#' @details Sampling is performed as per Michael, Schucany, Haas (1976). The quantile and entropy
#' functions are omitted as no closed form analytic expression could be found.
#'
#' @references
#'
#' Michael, John R.; Schucany, William R.; Haas, Roy W. (May 1976). "Generating Random Variates Using
#' Transformations with Multiple Roots". The American Statistician. 30 (2): 88–90.
#' doi:10.2307/2683801. JSTOR 2683801.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x = Wald$new(mean = 2, shape = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(shape = 3))
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
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
# Wald Distribution Definition
#-------------------------------------------------------------
Wald <- R6::R6Class("Wald", inherit = SDistribution, lock_objects = F)
Wald$set("public","name","Wald")
Wald$set("public","short_name","Wald")
Wald$set("public","traits",list(type = PosReals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Wald$set("public","description","Wald Probability Distribution.")
Wald$set("public","package","distr6")

Wald$set("public","mean",function(){
  return(self$getParameterValue("mean"))
})
Wald$set("public","var",function(){
  return(self$getParameterValue("mean")^3/self$getParameterValue("shape"))
})
Wald$set("public","skewness",function(){
  return(3 * (self$getParameterValue("mean")/self$getParameterValue("shape"))^0.5)
})
Wald$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(15 * self$getParameterValue("mean")/self$getParameterValue("shape"))
  else
    return(15 * self$getParameterValue("mean")/self$getParameterValue("shape") + 3)
})
Wald$set("public", "mgf", function(t){
  mean <- self$getParameterValue("mean")
  shape <- self$getParameterValue("shape")
  return(exp(shape/mean * (1 - sqrt(1 - 2*mean^2*t/shape))))
})
Wald$set("public", "cf", function(t){
  mean <- self$getParameterValue("mean")
  shape <- self$getParameterValue("shape")
  return(exp(shape/mean * (1 - sqrt(1 - 2*mean^2*1i*t/shape))))
})
Wald$set("public","mode",function(){
  mean <- self$getParameterValue("mean")
  shape <- self$getParameterValue("shape")
  return(mean * ((1 + (9*mean^2)/(4*shape^2))^0.5 - (3*mean)/(2*shape)))
})

Wald$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mean)) lst = c(lst, list(mean = paramlst$mean))
  if(!is.null(paramlst$shape)) lst = c(lst, list(shape = paramlst$shape))
  return(lst)
})

Wald$set("public","initialize",function(mean = 1, shape = 1,
                                          decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mean, shape, verbose)
  self$setParameterValue(list(mean = mean, shape = shape))

  pdf <- function(x1){
    mean <- self$getParameterValue("mean")
    shape <- self$getParameterValue("shape")
    return((shape/(2*pi*x1^3))^0.5 * exp((-shape*(x1-mean)^2)/(2*mean^2*x1)))
  }
  cdf <- function(x1){
    mean <- self$getParameterValue("mean")
    shape <- self$getParameterValue("shape")
    return(pnorm(sqrt(shape/x1)*(x1/mean-1)) + exp(2*shape/mean)*pnorm(-sqrt(shape/x1)*(x1/mean + 1)))
  }
  rand <- function(n){
    mean <- self$getParameterValue("mean")
    shape <- self$getParameterValue("shape")
    y = rnorm(n)^2
    x = mean + (mean^2*y)/(2*shape) - (mean/2*shape)*sqrt(4*mean*shape*y + mean^2*y^2)
    z = runif(n)

    rand = x
    rand[z > mean/(mean+x)] = mean^2/x[z > mean/(mean+x)]
    return(rand)
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf,
                   rand = rand, support = PosReals$new(), distrDomain = PosReals$new(),
                   symmetric = TRUE)
  invisible(self)
})
