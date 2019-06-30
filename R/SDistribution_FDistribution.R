#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# F Distribution Documentation
#-------------------------------------------------------------
#' @title F Distribution
#'
#' @description Mathematical and statistical functions for the F distribution parameterised
#' with a pair of degrees of freedom. The F distribution is defined by the pdf,
#' \deqn{f(x) = \Gamma((\mu + \nu)/2) / (\Gamma(\mu/2) \Gamma(\nu/2)) (\mu/\nu)^(\mu/2) x^(\mu/2 - 1) (1 + (\mu/\nu) x)^-(\mu + \nu)/2}
#' where \eqn{\mu,\nu > 0} are the degrees of freedom.
#'
#' @details \code{cf} is omitted as no closed form expression could be found.
#'
#' @name FDistribution
#'
#' @section Constructor: FDistribution$new(df1 = 1, df2 = 1, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{df1, df2} \tab numeric \tab degrees of freedom. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The F distribution is parameterised with a pair of
#' degrees of freedom, df1 and df2. Default parameterisation is with df1 = 1, df2 = 1.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' x <- FDistribution$new(df1 = 1, df2 = 3)
#'
#' # Update parameters
#' x$setParameterValue(list(df2 = 10))
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
# F Distribution Definition
#-------------------------------------------------------------
FDistribution <- R6::R6Class("FDistribution", inherit = SDistribution, lock_objects = FALSE)
FDistribution$set("public", "name", "FDistribution")
FDistribution$set("public", "short_name", "F")
FDistribution$set("public", "traits", list(type = PosReals$new(zero = TRUE),
                                           valueSupport = "continuous",
                                           variateForm = "univariate"))
FDistribution$set("public", "description", "F Probability Distribution")
FDistribution$set("public", "package", "stats")

FDistribution$set("public", "mean", function(){
  if(self$getParameterValue("df2") > 2)
    return(self$getParameterValue("df2")/(self$getParameterValue("df2") - 2))
  else
    return(NaN)
})
FDistribution$set("public", "var", function(){
  if(self$getParameterValue("df2") > 4)
    return((2*self$getParameterValue("df2")^2*(self$getParameterValue("df1") + self$getParameterValue("df2") - 2))/
             (self$getParameterValue("df1")*(self$getParameterValue("df2") - 2)^2*(self$getParameterValue("df2") - 4)))
  else
    return(NaN)
})
FDistribution$set("public", "skewness", function(){
  if (self$getParameterValue("df2") > 6){
    df1 <- self$getParameterValue("df1")
    df2 <- self$getParameterValue("df2")
    return(((2*df1 + df2 - 2)*sqrt(8*(df2 - 4)))/(((df2 - 6)*sqrt(df1*(df1 + df2 - 2)))))
  }else
    return(NaN)
})
FDistribution$set("public", "kurtosis", function(excess = TRUE){
  if (self$getParameterValue("df2") > 8){
    df1 <- self$getParameterValue("df1")
    df2 <- self$getParameterValue("df2")
    exkurtosis <- (12*(df1*(5*df2 - 22)*(df1 + df2 - 2) + (df2 - 4)*(df2 - 2)^2))/
      (df1*(df2 - 6)*(df2 - 8)*(df1 + df2 - 2))
    if(excess == TRUE)
      return(exkurtosis)
    else
      return(exkurtosis + 3)
  } else{
    return(NaN)
  }
})
FDistribution$set("public", "entropy", function(base = 2){
  df1 <- self$getParameterValue("df1")
  df2 <- self$getParameterValue("df2")
  return(log(gamma(df1/2), base) + log(gamma(df2/2), base) - log(gamma((df1 + df2)/2), base) +
           log(df1/df2, base) + (1 - df1/2)*digamma(1 + df1/2) - (1 + df2/2)*digamma(1 + df2/2) +
           ((df1 + df2)/2)*digamma((df1 + df2)/2))
})
FDistribution$set("public", "mgf", function(t){
  return(NaN)
})
FDistribution$set("public", "mode", function(){
  if(self$getParameterValue("df1") > 2)
    return(((self$getParameterValue("df1") - 2)*self$getParameterValue("df2"))/
             (self$getParameterValue("df1")*(self$getParameterValue("df2") + 2)))
  else
    return(NaN)
})

FDistribution$set("public", "setParameterValue", function(lst, error = "warn"){
  super$setParameterValue(lst, error)
  if (self$getParameterValue("df1") == 1)
    private$.properties$support <- PosReals$new(zero = FALSE)
  else
    private$.properties$support <- PosReals$new(zero = TRUE)
})
FDistribution$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if (!is.null(paramlst$df1)) lst = c(lst, list(df1 = paramlst$df1))
  if (!is.null(paramlst$df2)) lst = c(lst, list(df2 = paramlst$df2))
  return(lst)
})

FDistribution$set("public", "initialize", function(df1 = 1, df2 = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df1, df2, verbose)
  self$setParameterValue(list(df1 = df1, df2 = df2))

  pdf <- function(x1) df(x1, df1, df2)
  cdf <- function(x1) pf(x1, df1, df2)
  quantile <- function(p) qf(p, df1, df2)
  rand <- function(n) rf(n, df1, df2)

  if (df1 == 1)
    support <- PosReals$new(zero = FALSE)
  else
    support <- PosReals$new(zero = TRUE)

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = support, distrDomain = PosReals$new(zero = TRUE),
                   symmetric = FALSE)
})
