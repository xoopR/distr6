#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# DiscreteUniform Distribution Documentation
#-------------------------------------------------------------
#' @title DiscreteUniform Distribution
#'
#' @description Mathematical and statistical functions for the DiscreteUniform distribution parameterised
#' with lower and upper limits.
#'
#' @name DiscreteUniform
#'
#' @section Constructor: DiscreteUniform$new(lower = 0, upper = 1, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{lower} \tab integer \tab probability of success. \cr
#' \code{upper} \tab integer \tab probability of success. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' }
#'
#' @section Constructor Details: The DiscreteUniform distribution is parameterised with lower and
#' upper limits as integers.
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#'
#' @export
NULL
#-------------------------------------------------------------
# DiscreteUniform Distribution Definition
#-------------------------------------------------------------
DiscreteUniform <- R6::R6Class("DiscreteUniform", inherit = Distribution, lock_objects = F)
DiscreteUniform$set("public","name","DiscreteUniform")
DiscreteUniform$set("public","short_name","DUnif")
DiscreteUniform$set("public","traits",list(type = Integers$new(),
                                     valueSupport = "discrete",
                                     variateForm = "univariate"))
DiscreteUniform$set("public","description","DiscreteUniform Probability Distribution.")

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
DiscreteUniform$set("public","setParameterValue",function(lst, error = "warn"){
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= lst[["upper"]])
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"))
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"))

  super$setParameterValue(lst, error)
  private$.properties$support <- Set$new(self$getParameterValue("lower"):self$getParameterValue("upper"))
})

DiscreteUniform$set("public","initialize",function(lower = 0, upper = 1, decorators = NULL){

  checkmate::assert(lower <= upper)

  private$.parameters <- ParameterSet$new(id = list("lower","upper", "N"),
                                          value = list(0, 1, (upper - lower + 1)),
                                          lower = list(-Inf, -Inf, -Inf),
                                          upper = list(Inf, Inf, Inf),
                                          class = list("integer","integer","integer"),
                                          settable = list(TRUE, TRUE, FALSE),
                                          updateFunc = list(NULL, NULL,
                                                            "self$getParameterValue('upper') - self$getParameterValue('lower') + 1"),
                                          description = list("Lower distribution limit.",
                                                             "Upper distribution limit.",
                                                             "Distribution width."))

  self$setParameterValue(list(lower = lower, upper = upper))

  pdf = function(x1) return(1 / self$getParameterValue("N"))
  cdf = function(x1) return((x1 - self$getParameterValue("lower") + 1)/ self$getParameterValue("N"))
  quantile = function(p) return(self$getParameterValue("lower") + floor(p * self$getParameterValue("N")))
  rand = function(n) return(self$quantile(runif(n)))

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Set$new(lower, upper),
                   distrDomain = Integers$new(zero = T), symmetric = FALSE)
  invisible(self)
})
