#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Uniform Distribution Documentation
#-------------------------------------------------------------
#' @title Uniform Distribution
#'
#' @description Mathematical and statistical functions for the Uniform distribution parameterised
#' with lower bound and upper bound.
#'
#' @name Uniform
#'
#' @section Constructor: Uniform$new(a = NULL, b = NULL, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{a} \tab numeric \tab a, lower bound. \cr
#' \code{b} \tab numeric \tab b, upper bound. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#'#' \code{...} \tab ANY \tab additional arguments for Distribution constructor. See details. \cr
#' }
#'
#' @section Constructor Details: The Logistic distribution can either be parameterised with scaleiance,
#' standard deviation or precision. If none are provided then scale parameterisation is used with scale = 1.
#'
#'
#' @inheritSection Distribution Public scaleiables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{location()} \tab \code{\link{location.Distribution}} \cr
#'   \code{scale()} \tab \code{\link{scale}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
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
# Uniform Distribution Definition
#-------------------------------------------------------------
library(distr6)
library(R6)
remotes::install_github('RaphaelS1/distr6',force=TRUE)

Uniform <- R6::R6Class("Uniform", inherit = Distribution, lock_objects = F)
Uniform$set("public","name","Uniform")
Uniform$set("public","short_name","Uni")
Uniform$set("public","traits",list(type = Reals$new(),
                                  valueSupport = "continuous",
                                  variateForm = "univariate"))
Uniform$set("public","properties",list(support = Reals$new(zero = T),
                                      distrDomain = Reals$new(zero = T),
                                      symmetry  = "symmetric"))

Uniform$set("private",".pdf",function(x1, log = FALSE){
  dunif(x1, self$getParameterValue("a"), self$getParameterValue("b"), log)
})

Uniform$set("private",".cdf",function(q, lower.tail = TRUE, log.p = FALSE){
  punif(q, self$getParameterValue("a"), self$getParameterValue("b"), lower.tail, log.p)
})

Uniform$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  qunif(p, self$getParameterValue("a"), self$getParameterValue("b"), lower.tail, log.p)
})

Uniform$set("private",".rand",function(n){
  runif(n, self$getParameterValue("a"), self$getParameterValue("b"))
})

Uniform$set("public","expectation",function(){
 (1/2(self$getParameterValue("a")+self$getParameterValue("b")))
})

Uniform$set("public","var",function(){
  1/12((self$getParameterValue(b)-self$getParameterValue("a"))^2)
})

Uniform$set("public","skewness",function() return(0))

Uniform$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-6/5)
  else
    return(3)
})
Uniform$set("public","entropy",function(){
  return(ln( self$getParameterValue("b")-self$getParameterValue("a")))
})

Uniform$set("public", "mgf", function(t){
  if(t!=0)
  return((exp(self$getParameterValue("b") * t) - exp(self$getParameterValue("a") * t )))/(t*(self$getParameterValue("b")-self$getParameterValue("a")))
  else
  return(1)
})

Uniform$set("public", "cf", function(t){
  if(t!=0)
  return(exp((1i * self$getParameterValue("b") * t) - exp((self$getParameterValue("a") * t * 1i)))/1i*t*(self$getParameterValue("b")-self$getParameter("a")))
  else
  return(1)
})


Uniform$set("public","survival",function(x1, log.p = FALSE){
  self$cdf(x1, lower.tail = FALSE, log.p)
})

Uniform$set("public","hazard",function(x1){
  self$pdf(x1)/self$survival(x1)
})

Uniform$set("public","cumHazard",function(x1){
  -self$cdf(x1, log.p = TRUE)
})

Uniform$set("public","mode",function() {
  A<-self$getParameterValue("a")
  B<-self$getParameterValue("b")
  return(sample(A,B))})
Uniform$set("private",".parameters", NULL)
Uniform$set("public","initialize",function(a = NULL, b = NULL, decorators = NULL,...){
  
  a.bool = FALSE
  b.bool = FALSE
  
  if(is.null(a) & is.null(b) ){
    message("a and b are missing.  a=0,b=1 parameterisation used.")
    a = 0 
    b = 1
  } else if(!is.null(a) & (!is.null(b))){
    message("Multiple parameterisations provided. a and b parameterisation used.")
    a = a
    b = b
   } else if(is.null(a) & !is.null(b)){
    message("a is missing. b parameterisation used.")
    a = NULL
    b = b
   } else if(!is.null(a) & is.null(b)){
     message("a is provided. b is missing.")
     a = a
     b = NULL
  }
  
  
 private$.parameters <- ParameterSet$new(id = list("a","b"),
                                          value = list(0, 1),
                                          lower = list(-Inf, -Inf),
                                          upper = list(Inf, Inf),
                                          class = list("numeric","numeric"),
                                          settable = list(TRUE, TRUE),
                                         description = list("a-lower bound","b-upper bound")
                                
                                         
                                                                      )
  
  if (!is.null(a)) self$setParameterValue(list(a = a))
  else if(!is.null(b)) self$setParameterValue(list(b = b))

  
  super$initialize(decorators = decorators,...)
  invisible(self)
})
devtools::document()