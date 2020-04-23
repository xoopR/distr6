
#-------------------------------------------------------------
# Noncentral Beta Distribution Documentation
#-------------------------------------------------------------
#' @name BetaNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName BetaNoncentral
#' @templateVar DistName Noncentral Beta
#' @templateVar uses as the prior in Bayesian modelling
#' @templateVar params two shape parameters, \eqn{\alpha, \beta}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-\lambda/2) \sum_{r=0}^\infty ((\lambda/2)^r/r!) (x^{\alpha+r-1}(1-x)^{\beta-1})/B(\alpha+r, \beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0, \lambda \ge 0}, where \eqn{B} is the Beta function
#' @templateVar distsupport \eqn{[0, 1]}
#' @templateVar omittedVars \code{mean}, \code{variance}, \code{skewness}, \code{kurtosis}, \code{entropy}, \code{mode}, \code{mgf} and \code{cf}
#' @templateVar constructor shape1 = 1, shape2 = 1, location = 0
#' @templateVar arg1 \code{shape1, shape2} \tab numeric \tab positive shape parameter. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab location (ncp in rstats). \cr
#' @templateVar constructorDets  \code{shape1}, \code{shape2} as positive numerics, \code{location} as non-negative numeric.
#'
#' @examples
#' x = BetaNoncentral$new(shape1 = 2, shape2 = 5, location = 3)
#'
#' # Update parameters
#' x$setParameterValue(shape1 = 1)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Noncentral Beta Distribution Definition
#-------------------------------------------------------------
BetaNoncentral <- R6Class("BetaNoncentral", inherit = SDistribution, lock_objects = F)
BetaNoncentral$set("public","name","BetaNoncentral")
BetaNoncentral$set("public","short_name","BetaNC")
BetaNoncentral$set("public","description","Noncentral Beta Probability Distribution.")
BetaNoncentral$set("public","packages","stats")

BetaNoncentral$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$shape1)) lst = c(lst,list(shape1 = paramlst$shape1))
  if(!is.null(paramlst$shape2)) lst = c(lst,list(shape2 = paramlst$shape2))
  if(!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})
BetaNoncentral$set("private", ".pdf", function(x, log = FALSE){
  shape1 = self$getParameterValue("shape1")
  shape2 = self$getParameterValue("shape2")
  ncp = self$getParameterValue("location")

  call_C_base_pdqr(fun = "dbeta",
                   x = x,
                   args = list(shape1 = unlist(shape1),
                               shape2 = unlist(shape2),
                               ncp = unlist(ncp)),
                   log = log,
                   vec = test_list(shape1))
})
BetaNoncentral$set("private", ".cdf", function(x, lower.tail = TRUE, log.p = FALSE){
  shape1 = self$getParameterValue("shape1")
  shape2 = self$getParameterValue("shape2")
  ncp = self$getParameterValue("location")

  call_C_base_pdqr(fun = "pbeta",
                   x = x,
                   args = list(shape1 = unlist(shape1),
                               shape2 = unlist(shape2),
                               ncp = unlist(ncp)),
                   lower.tail = lower.tail,
                   log = log.p,
                   vec = test_list(shape1))
})
BetaNoncentral$set("private", ".quantile", function(p, lower.tail = TRUE, log.p = FALSE){
  shape1 = self$getParameterValue("shape1")
  shape2 = self$getParameterValue("shape2")
  ncp = self$getParameterValue("location")

  call_C_base_pdqr(fun = "qbeta",
                   x = p,
                   args = list(shape1 = unlist(shape1),
                               shape2 = unlist(shape2),
                               ncp = unlist(ncp)),
                   lower.tail = lower.tail,
                   log = log.p,
                   vec = test_list(shape1))
})
BetaNoncentral$set("private", ".rand", function(n){
  shape1 = self$getParameterValue("shape1")
  shape2 = self$getParameterValue("shape2")
  ncp = self$getParameterValue("location")

  call_C_base_pdqr(fun = "rbeta",
                   x = n,
                   args = list(shape1 = unlist(shape1),
                               shape2 = unlist(shape2),
                               ncp = unlist(ncp)),
                   vec = test_list(shape1))
})
BetaNoncentral$set("private", ".log", TRUE)

BetaNoncentral$set("public", "initialize", function(shape1 = 1, shape2 = 1, location = 0, decorators = NULL,
                                                    verbose = FALSE){

  private$.parameters <- getParameterSet.BetaNoncentral(self, shape1, shape2, locaiton, verbose)
  self$setParameterValue(shape1=shape1,shape2=shape2, location = location)

  super$initialize(decorators = decorators,
                   support = Interval$new(0, 1),
                   symmetric = if (shape1 == shape2) "symmetric" else "asymmetric",
                   type = PosReals$new(zero = T),
                   valueSupport ="continuous")
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "BetaNC", ClassName = "BetaNoncentral",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))

