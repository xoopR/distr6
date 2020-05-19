
#-------------------------------------------------------------
# Rayleigh Distribution Documentation
#-------------------------------------------------------------
#' @name Rayleigh
#' @template SDist
#' @templateVar ClassName Rayleigh
#' @templateVar DistName Rayleigh
#' @templateVar uses to model random complex numbers.
#' @templateVar params mode (or scale), \eqn{\alpha},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = x/\alpha^2 exp(-x^2/(2\alpha^2))}
#' @templateVar paramsupport \eqn{\alpha > 0}
#' @templateVar distsupport \eqn{[0, \infty)}
#' @templateVar omittedVars \code{cf} and \code{mgf}
#' @templateVar constructor mode = 1
#' @templateVar arg1 \code{mode} \tab numeric \tab mode, scale parameter. \cr
#' @templateVar constructorDets \code{mode} as a non-negative numeric.
#'
#' @examples
#' x <- Rayleigh$new(mode = 2)
#'
#' # Update parameters
#' x$setParameterValue(mode = 4)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(1:4)
#' x$cdf(2)
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
# Rayleigh Distribution Definition
#-------------------------------------------------------------
Rayleigh <- R6Class("Rayleigh", inherit = SDistribution, lock_objects = F)
Rayleigh$set("public","name","Rayleigh")
Rayleigh$set("public","short_name","Rayl")
Rayleigh$set("public","description","Rayleigh Probability Distribution.")
Rayleigh$set("public","packages", "extraDistr")

Rayleigh$set("public","mean",function(){
  return(self$getParameterValue("mode")*sqrt(pi/2))
})
Rayleigh$set("public","mode",function(which = NULL){
  return(self$getParameterValue("mode"))
})
Rayleigh$set("public","variance",function(){
  return((4-pi)/2 * self$getParameterValue("mode")^2)
})
Rayleigh$set("public","skewness",function(){
  return((2*sqrt(pi)*(pi-3))/((4-pi)^(3/2)))
})
Rayleigh$set("public","kurtosis",function(excess = TRUE){
  if(excess)
    return(-(6*pi^2 - 24*pi + 16)/(4-pi)^2)
  else
    return(-(6*pi^2 - 24*pi + 16)/(4-pi)^2 + 3)
})
Rayleigh$set("public","entropy",function(base = 2){
  return(1 + log(self$getParameterValue("mode")/sqrt(2), base) - digamma(1)/2)
})
Rayleigh$set("public", "pgf", function(z){
  return(NaN)
})

Rayleigh$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$mode)) lst = c(lst, list(mode = paramlst$mode))
  return(lst)
})
Rayleigh$set("private",".pdf", function(x){
  if (checkmate::testList(self$getParameterValue("mode"))) {
    mapply(
      extraDistr::drayleigh,
      sigma = self$getParameterValue("mode"),
      MoreArgs = list(x = x, log = log)
    )
  } else {
    extraDistr::drayleigh(
      x,
      sigma = self$getParameterValue("mode"),
      log = log
    )
  }
})
Rayleigh$set("private",".cdf", function(x){
  if (checkmate::testList(self$getParameterValue("mode"))) {
    mapply(
      extraDistr::prayleigh,
      sigma = self$getParameterValue("mode"),
      MoreArgs = list(
        q = x,
        mode.tail = mode.tail,
        log.p = log.p
      )
    )
  } else {
    extraDistr::prayleigh(
      x,
      sigma = self$getParameterValue("mode"),
      mode.tail = mode.tail,
      log.p = log.p
    )
  }
})
Rayleigh$set("private",".quantile", function(p){
  if (checkmate::testList(self$getParameterValue("mode"))) {
    mapply(
      extraDistr::qrayleigh,
      sigma = self$getParameterValue("mode"),
      MoreArgs = list(
        p = p,
        mode.tail = mode.tail,
        log.p = log.p
      )
    )
  } else {
    extraDistr::qrayleigh(
      p,
      sigma = self$getParameterValue("mode"),
      mode.tail = mode.tail,
      log.p = log.p
    )
  }
})
Rayleigh$set("private",".rand", function(n){
  if (checkmate::testList(self$getParameterValue("mode"))) {
    mapply(
      extraDistr::rrayleigh,
      sigma = self$getParameterValue("mode"),
      MoreArgs = list(n = n)
    )
  } else {
    extraDistr::rrayleigh(
      n,
      sigma = self$getParameterValue("mode")
    )
  }
})
Rayleigh$set("private",".log", TRUE)
Rayleigh$set("private", ".traits", list(valueSupport = "continuous", variateForm = "univariate"))

Rayleigh$set("public","initialize",function(mode = 1, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, mode, verbose)
  self$setParameterValue(mode = mode)

  super$initialize(decorators = decorators,
                   support = PosReals$new(zero = T),
                   type = PosReals$new(zero = T))
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Rayl", ClassName = "Rayleigh",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "extraDistr"))
