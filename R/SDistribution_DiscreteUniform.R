#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# DiscreteUniform Distribution Documentation
#-------------------------------------------------------------
#' @name DiscreteUniform
#' @template SDist
#' @templateVar ClassName DiscreteUniform
#' @templateVar DistName Discrete Uniform
#' @templateVar uses as a discrete variant of the more popular Uniform distribution, used to model events with an equal probability of occurring (e.g. role of a die)
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(b - a + 1)}
#' @templateVar paramsupport \eqn{a, b \ \in \ Z; \ b \ge a}{a, b \epsilon Z; b \ge a}
#' @templateVar distsupport \eqn{\{a, a + 1,..., b\}}{{a, a + 1,..., b}}
#' @templateVar constructor lower = 0, upper = 1
#' @templateVar arg1 \code{lower} \tab integer \tab lower distribution limit. \cr
#' @templateVar arg2 \code{upper} \tab integer \tab upper distribution limit. \cr
#' @templateVar constructorDets \code{lower} and \code{upper} as whole numbers.
#' @templateVar additionalSeeAlso \code{\link{Uniform}} for the (continuous) Uniform distribution.
#'
#' @examples
#' x <- DiscreteUniform$new(lower = -10, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(lower = 2, upper = 7)
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
# DiscreteUniform Distribution Definition
#-------------------------------------------------------------
DiscreteUniform <- R6::R6Class("DiscreteUniform", inherit = SDistribution, lock_objects = F)
DiscreteUniform$set("public","name","DiscreteUniform")
DiscreteUniform$set("public","short_name","DUnif")
DiscreteUniform$set("public","description","DiscreteUniform Probability Distribution.")
DiscreteUniform$set("public","package","distr6")

DiscreteUniform$set("public","mean",function(){
  return((self$getParameterValue("lower") + self$getParameterValue("upper")) / 2)
})
DiscreteUniform$set("public","variance",function(){
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
DiscreteUniform$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= lst[["upper"]], .var.name = "lower must be <= upper")
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"), .var.name = "lower must be <= upper")
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"), .var.name = "upper must be >= lower")

  super$setParameterValue(lst = lst, error = error)
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
  self$setParameterValue(lower = lower, upper = upper)

  pdf = function(x1) return(1 / self$getParameterValue("N"))
  cdf = function(x1) return((x1 - self$getParameterValue("lower") + 1)/ self$getParameterValue("N"))
  quantile = function(p) return(self$getParameterValue("lower") + floor(p * self$getParameterValue("N")))
  rand = function(n) return(self$quantile(runif(n)))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(lower:upper),
                   symmetric = TRUE, type = Integers$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "DUnif", ClassName = "DiscreteUniform",
                                                     Type = "\u2124", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))

