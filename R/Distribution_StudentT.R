#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Student's t Distribution Documentation
#-------------------------------------------------------------
#' @title Student's t Distribution
#' 
#' @description Mathematical and statistical functions for the Student's t distribution parameterised
#' with df degrees of freedom.
#' 
#' @name StudentT
#'
#' @section Constructor: StudentT$new(df = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{df} \tab numeric \tab degrees of freedom. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Student's t distribution can be parameterised with df
#' degrees of freedom. Default parameterisation is with df = 1.
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Normal Statistical Methods
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
#'   \code{entropy(base = exp(1))} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#'
#' @export
NULL
#-------------------------------------------------------------
# Student's t Distribution Definition
#-------------------------------------------------------------
StudentT <- R6::R6Class("StudentT", inherit = SDistribution, lock_objects = F)
StudentT$set("public","name","StudentT")
StudentT$set("public","short_name","T")
StudentT$set("public","traits",list(type = Reals$new(zero = T),
                                    valueSupport = "continuous",
                                    variateForm = "univariate"))
StudentT$set("public","description","Student's t Probability Distribution.")

StudentT$set("public","mean",function(){
  v <- self$getParameterValue("df")
  if(v > 1)
    return(0)
  else
    return(NaN)
})

StudentT$set("public","var",function(){
  v <- self$getParameterValue("df")
  if(v > 2)
    return(v/(v-2))
  else if(v > 1 & v <= 2)
    return(Inf)
  else
    return(NaN)
})

StudentT$set("public","skewness",function(){
  v <- self$getParameterValue("df")
  if(v > 3)
    return(0)
  else
    return(NaN)
})

StudentT$set("public","kurtosis",function(excess = TRUE){
  v <- self$getParameterValue("df")
  if(v > 4)
    exkurtosis = 6/(v-4)
  else if(v > 2 & v <= 4)
    exkurtosis = Inf
  else
    exkurtosis = NaN
  if(excess)
    return(exkurtosis)
  else
    return(exkurtosis + 3)
  
})

StudentT$set("public","entropy",function(base = exp(1)){
  v <- self$getParameterValue("df")
  ((v+1)/2)*(digamma((1+v)/2) - digamma(v/2)) + log(sqrt(v)*beta(v/2, 1/2), base)
})

StudentT$set("public", "mgf", function(t) return(NaN))

StudentT$set("public", "cf", function(t){
  v <- self$getParameterValue("df")
  return(besselK(sqrt(v)*abs(t), v/2) * ((sqrt(v)*abs(t))^(v/2)) / (gamma(v/2)*2^(v/2-1)))
})

StudentT$set("public","mode",function() return(0))

StudentT$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$df)) lst = c(lst, list(df = paramlst$df))
  return(lst)
})

StudentT$set("public","initialize",function(df = 1, decorators = NULL, verbose = FALSE){
  
  private$.parameters <- getParameterSet(self, df, verbose)
  self$setParameterValue(list(df = df))
  
  pdf <- function(x1) dt(x1, self$getParameterValue("df"))
  cdf <- function(x1) pt(x1, self$getParameterValue("df"))
  quantile <- function(p) qt(p, self$getParameterValue("df"))
  rand <- function(n) rt(n, self$getParameterValue("df"))
  
  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Reals$new(zero = T), distrDomain = Reals$new(zero = T),
                   symmetric  = TRUE)
  invisible(self)
})