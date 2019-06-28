#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Triangular Distribution Documentation
#-------------------------------------------------------------
#' @title (Symmetric) Triangular Distribution
#'
#' @description Mathematical and statistical functions for the Triangular distribution parameterised
#' with lower limit, upper limit and mode and defined by the pdf,
#' \deqn{f(x) = 0, x < a}
#' \deqn{f(x) = 2(x-a)/((b-a)(c-a)), a \le x < c}
#' \deqn{f(x) = 2/(b-a), x = c}
#' \deqn{f(x) = 2(b-x)/((b-a)(b-c)), c < x \le b}
#' \deqn{f(x) = 0, x > b}
#'
#' where \eqn{a \epsilon R} is the lower limit, \eqn{b > a} is the upper limit and \eqn{a \le c \le b}
#' is the mode.
#'
#' @name Triangular
#'
#' @section Constructor: Triangular$new(lower = 0, upper = 1, mode = 0.5, symmetric = FALSE,
#' decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{lower} \tab numeric \tab lower limit. \cr
#' \code{upper} \tab numeric \tab upper limit. \cr
#' \code{mode} \tab numeric \tab mode. \cr
#' \code{symmetric} \tab logical \tab see details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The (non-symmetric) Triangular distribution is parameterised with
#' lower = 0, upper = 1 and mode = 0.5 by default. If \code{symmetric = TRUE} then the \code{mode}
#' parameter is not-settable and is defined by \eqn{mode = (lower + upper) /2}, this cannot be changed
#' after construction.If \code{symmetric = FALSE} (default) then \eqn{mode} is settable after
#' construction.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)
#' Triangular$new(lower = 2, upper = 5, symmetric = FALSE) # Note mode defaults to a symmetric shape
#' Triangular$new(lower = 2, upper = 5, mode = 4)
#'
#' # You can view the type of Triangular distribution with \code{description}
#' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)$description
#' Triangular$new(lower = 2, upper = 5, symmetric = FALSE)$description
#'
#' x = Triangular$new(lower = -1, upper = 1)
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
# Triangular Distribution Definition
#-------------------------------------------------------------
Triangular <- R6::R6Class("Triangular", inherit = SDistribution, lock_objects = F)
Triangular$set("public","name","Triangular")
Triangular$set("public","short_name","Tri")
Triangular$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Triangular$set("public","package","distr6")
Triangular$set("private",".type","symmetric")

Triangular$set("public","mean",function(){
  return((self$getParameterValue("lower") + self$getParameterValue("upper") + self$getParameterValue("mode"))/3)
})
Triangular$set("public","var",function(){
  return((self$getParameterValue("lower")^2 + self$getParameterValue("upper")^2 +
            self$getParameterValue("mode")^2 - self$getParameterValue("lower")*self$getParameterValue("upper") -
            self$getParameterValue("lower")*self$getParameterValue("mode")-
            self$getParameterValue("upper")*self$getParameterValue("mode"))/18)
})
Triangular$set("public","skewness",function(){
  lower <- self$getParameterValue("lower")
  upper <- self$getParameterValue("upper")
  mode <- self$getParameterValue("mode")

  num <- sqrt(2)*(lower+upper-2*mode)*(2*lower-upper-mode)*(lower-2*upper+mode)
  den <- 5*(lower^2+upper^2+mode^2-lower*upper-lower*mode-upper*mode)^1.5
  return(num/den)
  })
Triangular$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-0.6)
  else
    return(2.4)
})
Triangular$set("public","entropy",function(base = 2){
  return(0.5 * log((self$getParameterValue("upper")-self$getParameterValue("lower"))/2, base))
})
Triangular$set("public", "mgf", function(t){
  lower <- self$getParameterValue("lower")
  upper <- self$getParameterValue("upper")
  mode <- self$getParameterValue("mode")

  num <- 2 * ((upper-mode)*exp(lower*t) - (upper-lower)*exp(mode*t) + (mode-lower)*exp(upper*t))
  den <- (upper-lower) * (mode-lower) * (upper-mode) * t^2

  return(num/den)
})
Triangular$set("public", "cf", function(t){
  lower <- self$getParameterValue("lower")
  upper <- self$getParameterValue("upper")
  mode <- self$getParameterValue("mode")

  num <- -2 * ((upper-mode)*exp(1i*lower*t) - (upper-lower)*exp(1i*mode*t) + (mode-lower)*exp(1i*upper*t))
  den <- (upper-lower) * (mode-lower) * (upper-mode) * t^2

  return(num/den)
})
Triangular$set("public","mode",function(){
  return(self$getParameterValue("mode"))
})

Triangular$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$lower)) lst = c(lst, list(lower = paramlst$lower))
  if(!is.null(paramlst$upper)) lst = c(lst, list(upper = paramlst$upper))
  if(private$.type != "symmetric")
    if(!is.null(paramlst$mode)) lst = c(lst, list(mode = paramlst$mode))
  return(lst)
})
Triangular$set("public","setParameterValue",function(lst, error = "warn"){
  if("lower" %in% names(lst) & "upper" %in% names(lst))
    checkmate::assert(lst[["lower"]] < lst[["upper"]], .var.name = "lower must be < upper")
  else if("lower" %in% names(lst))
    checkmate::assert(lst[["lower"]] < self$getParameterValue("upper"), .var.name = "lower must be < upper")
  else if("upper" %in% names(lst))
    checkmate::assert(lst[["upper"]] > self$getParameterValue("lower"), .var.name = "upper must be > lower")

  if(private$.type != "symmetric"){
    if("mode" %in% names(lst)){
      if("lower" %in% names(lst))
        checkmate::assert(lst[["mode"]] >= lst[["lower"]], .var.name = "mode must be >= lower")
      else
        checkmate::assert(lst[["mode"]] >= self$getParameterValue("lower"), .var.name = "mode must be >= lower")
      if("upper" %in% names(lst))
         checkmate::assert(lst[["mode"]] <= lst[["upper"]], .var.name = "mode must be <= upper")
      else
         checkmate::assert(lst[["mode"]] <= self$getParameterValue("upper"), .var.name = "mode must be <= upper")
    }
  }

  super$setParameterValue(lst = lst, error)

  private$.properties$support <- Interval$new(self$getParameterValue("lower"), self$getParameterValue("upper"))
  if(private$.type != "symmetric"){
    if(self$getParameterValue("mode") == (self$getParameterValue("lower") + self$getParameterValue("upper"))/2)
      private$.properties$symmetry <- "symmetric"
    else
      private$.properties$symmetry <- "asymmetric"
  }
  invisible(self)
})

Triangular$set("public","initialize",function(lower = 0, upper = 1, mode = (lower+upper)/2, symmetric = FALSE,
                                              decorators = NULL, verbose = FALSE){


  if(symmetric){
    description = "Symmetric Triangular Probability Distribution."
    symmetry = TRUE
    private$.type <- "symmetric"
  } else {
    description = "Triangular Probability Distribution."
    private$.type <- "asymmetric"
    if(mode == (lower + upper)/2)
      symmetry = TRUE
    else
      symmetry = FALSE
  }

  private$.parameters <- getParameterSet(self, lower, upper, mode, symmetric, verbose)
  self$setParameterValue(list(lower = lower, upper = upper, mode = mode))

  pdf <- function(x1){
    lower = self$getParameterValue("lower")
    upper = self$getParameterValue("upper")
    mode = self$getParameterValue("mode")
    pdf = x1

    pdf[lower <= x1 & x1 < mode] = (2*(pdf[lower <= x1 & x1 < mode]-lower)) /((upper-lower)*(mode-lower))
    pdf[x1 == mode] = 2/(upper-lower)
    pdf[x1 > mode & x1 <= upper] = (2*(upper-pdf[x1 > mode & x1 <= upper]))/((upper-lower)*(upper-mode))
    return(pdf)
  }
  cdf <- function(x1){
    lower = self$getParameterValue("lower")
    upper = self$getParameterValue("upper")
    mode = self$getParameterValue("mode")
    cdf = x1

    cdf[x1 == lower] = 0
    cdf[x1 == upper] = 1

    cdf[lower < x1 & x1 <= mode] = ((cdf[lower < x1 & x1 <= mode]-lower)^2) / ((upper-lower)*(mode-lower))
    cdf[x1 > mode & x1 < upper] = 1 - (((upper-cdf[x1 > mode & x1 < upper])^2)/((upper-lower)*(upper-mode)))
    return(cdf)
  }
  quantile <- function(p){
    lower = self$getParameterValue("lower")
    upper = self$getParameterValue("upper")
    mode = self$getParameterValue("mode")
    quantile = p

    quantile[p == 0] = lower
    quantile[p == 1] = upper

    quantile[0 < p & p <= (mode-lower)/(upper-lower)] = lower + sqrt((upper-lower)*(mode-lower)*quantile[0 < p & p <= (mode-lower)/(upper-lower)])
    quantile[(mode-lower)/(upper-lower) < p & p < 1] = upper - sqrt((1-quantile[(mode-lower)/(upper-lower) < p & p < 1])*(upper-lower)*(upper-mode))

    return(quantile)
  }
  rand <- function(n){
    return(self$quantile(runif(n)))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = Interval$new(lower, upper), distrDomain = Reals$new(),
                   symmetric = symmetry, description = description)
  invisible(self)
})
