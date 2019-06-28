#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# DiscreteUniform Distribution Documentation
#-------------------------------------------------------------
#' @title DiscreteUniform Distribution
#'
#' @description Mathematical and statistical functions for the Discrete Uniform distribution parameterised
#' with lower and upper limits. The Discrete Uniform distribution is defined by the pmf,
#' \deqn{f(x) = 1/n}
#' where \eqn{n = b - a + 1} is the interval width and \eqn{a, b \epsilon Z; b \ge a}, are the lower and
#' upper limits respectively.
#'
#' @name DiscreteUniform
#'
#' @section Constructor: DiscreteUniform$new(lower = 0, upper = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{lower} \tab integer \tab lower distribution limit. \cr
#' \code{upper} \tab integer \tab upper distribution limit. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The DiscreteUniform distribution is parameterised with lower and
#' upper limits as integers.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- DiscreteUniform$new(lower = -10, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(list(lower = 2, upper = 7))
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
# DiscreteUniform Distribution Definition
#-------------------------------------------------------------
DiscreteUniform <- R6::R6Class("DiscreteUniform", inherit = SDistribution, lock_objects = F)
DiscreteUniform$set("public","name","DiscreteUniform")
DiscreteUniform$set("public","short_name","DUnif")
DiscreteUniform$set("public","traits",list(type = Integers$new(),
                                     valueSupport = "discrete",
                                     variateForm = "univariate"))
DiscreteUniform$set("public","description","DiscreteUniform Probability Distribution.")
DiscreteUniform$set("public","package","distr6")

DiscreteUniform$set("public","mean",function(){
  return((self$getParameterValue("lower") + self$getParameterValue("upper")) / 2)
})
DiscreteUniform$set("public","var",function(){
  return(((self$getParameterValue("upper") - self$getParameterValue("lower") + 1)^2 - 1) / 12)
})
DiscreteUniform$set("public","skewness",function(){
  return(0)
})
DiscreteUniform$set("public","kurtosis",function(excess = TRUE){
  exkurtosis = (-6 * (self$getParameterValue("N")^2 + 1)) / (5 * (self$getParameterValue("N")^2 - 1))
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
})
DiscreteUniform$set("public","entropy",function(base = 2){
  return(log(self$getParameterValue("N"), base))
})
DiscreteUniform$set("public", "mgf", function(t){
  num = exp(t * self$getParameterValue("lower")) - exp((self$getParameterValue("upper")+1)*t)
  denom = self$getParameterValue("N") * (1 - exp(t))
  return(num/denom)
})
DiscreteUniform$set("public", "cf", function(t){
  num = exp(1i * t * self$getParameterValue("lower")) - exp((self$getParameterValue("upper")+1) * t * 1i)
  denom = self$getParameterValue("N") * (1 - exp(1i * t))
  return(num/denom)
})
DiscreteUniform$set("public","pgf",function(z){
  return(1/self$getParameterValue("N") * sum(z^(1:self$getParameterValue("N"))))
})
DiscreteUniform$set("public","mode",function(which="all"){
  if(which=="all")
    return(self$inf():self$sup())
  else
    return((self$inf():self$sup())[which])
})
DiscreteUniform$set("public","setParameterValue",function(lst, error = "warn"){
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= lst[["upper"]], .var.name = "lower must be <= upper")
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"), .var.name = "lower must be <= upper")
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"), .var.name = "upper must be >= lower")

  super$setParameterValue(lst, error)
  private$.properties$support <- Set$new(self$getParameterValue("lower"):self$getParameterValue("upper"))
  invisible(self)
})

DiscreteUniform$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$lower)) lst = c(lst, list(lower = paramlst$lower))
  if(!is.null(paramlst$upper)) lst = c(lst, list(upper = paramlst$upper))
  return(lst)
})

DiscreteUniform$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, lower, upper, verbose)
  self$setParameterValue(list(lower = lower, upper = upper))

  pdf = function(x1) return(1 / self$getParameterValue("N"))
  cdf = function(x1) return((x1 - self$getParameterValue("lower") + 1)/ self$getParameterValue("N"))
  quantile = function(p) return(self$getParameterValue("lower") + floor(p * self$getParameterValue("N")))
  rand = function(n) return(self$quantile(runif(n)))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(lower:upper),
                   distrDomain = Integers$new(zero = T), symmetric = TRUE)
  invisible(self)
})
