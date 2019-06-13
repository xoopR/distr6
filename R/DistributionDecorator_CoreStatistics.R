#' @title Core Statistical Methods for Distributions
#' @description Added functionality to distribution objects for numerical statistical
#'   methods. Including a generalised expectation function for more complex numerical calculations.
#'
#' @name CoreStatistics
#'
#' @section Constructor: CoreStatistics$new(dist)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{mgf(t)} \tab Moment generating function \tab \code{\link{mgf}} \cr
#' \code{pgf(t)} \tab Probability generating function \tab \code{\link{pgf}} \cr
#' \code{cf(t)} \tab Characteristic function \tab \code{\link{cf}} \cr
#' \code{entropy(base = 2)} \tab (Shannon) Entropy \tab \code{\link{entropy}} \cr
#' \code{skewness()} \tab Skewness \tab \code{\link{skewness}} \cr
#' \code{kurtosis(excess = TRUE)} \tab Kurtosis \tab \code{\link{kurtosis}} \cr
#' \code{kthmoment(type = "central")} \tab Kth Moment \tab \code{\link{kthmoment}} \cr
#' \code{genExp(trafo)} \tab Generalised Expectation \tab \code{\link{genExp}} \cr
#' \code{mode(which = 1)} \tab Mode \tab \code{\link{mode}} \cr
#' \code{var()} \tab Variance \tab \code{\link{var}} \cr
#' \code{cov()} \tab Covariance \tab \code{\link{cov}} \cr
#' \code{cor()} \tab Correlation \tab \code{\link{cor}} \cr
#' }
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations. See below for the methods added to a distribution after decorating with
#'  \code{CoreStatistics}.
#'
#' @seealso \code{\link{DistributionDecorator}}, \code{\link{decorate}} and \code{\link{ExoticStatistics}}
#'
#' @examples
#' x = Binomial$new()
#' decorate(x, CoreStatistics)
#' x$genExp()
#'
#' @examples
#' x = Binomial$new(decorators = CoreStatistics)
#' x$kthmoment(4)
#'
#' @export
NULL
CoreStatistics <- R6::R6Class("CoreStatistics", inherit = DistributionDecorator)

#-------------------------------------------------------------
# Public Methods - mgf
#-------------------------------------------------------------
#' @title Moment Generating Function
#' @name mgf
#' @description Moment generating function of a distribution
#'
#' @usage mgf(object, t)
#' @section R6 Usage: $mgf(t)
#'
#' @param object Distribution.
#' @param t integer to evaluate moment generating function at.
#'
#' @details The moment generating function is defined by
#' \deqn{mgf_X(t) = E_X[exp(xt)]}
#' where X is the distribution and E_X is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "mgf", function(t) {
  return(self$genExp(trafo = function(x) {return(exp(x*t))}))
})

#-------------------------------------------------------------
# Public Methods - cf
#-------------------------------------------------------------
#' @title Characteristic Function
#' @name cf
#' @description Characteristic function of a distribution
#'
#' @usage cf(object, t)
#' @section R6 Usage: $cf(t)
#'
#' @param object Distribution.
#' @param t integer to evaluate characteristic function at.
#'
#' @details The characteristic function is defined by
#' \deqn{cf_X(t) = E_X[exp(xti)]}
#' where X is the distribution and E_X is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#'@seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self)){
    return(self$genExp(trafo = function(x) {return(exp(x*t*1i))}))
  }
})

#-------------------------------------------------------------
# Public Methods - pgf
#-------------------------------------------------------------
#' @title Probability Generating Function
#' @name pgf
#' @description Probability generating function of a distribution
#'
#' @usage pgf(object, z)
#' @section R6 Usage: $pgf(z)
#'
#' @param object Distribution.
#' @param z integer to evaluate characteristic function at.
#'
#' @details The probability generating function is defined by
#' \deqn{pgf_X(z) = E_X[exp(z^x)]}
#' where X is the distribution and E_X is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#'@seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$genExp(trafo = function(x) {return(z^x)})
    return(x)
  }
})

#-------------------------------------------------------------
# Public Methods - Entropy
#-------------------------------------------------------------
#' @title Distribution Entropy
#' @name entropy
#' @description (Information) Entropy of a distribution
#'
#' @param object Distribution.
#' @param base base of the entropy logarithm, default = 2 (Shannon entropy)
#'
#' @usage entropy(object, base = 2)
#' @section R6 Usage: $entropy(base = 2)
#'
#' @details The entropy of a (discrete) distribution is defined by
#' \deqn{- sum f_X * log(f_X)}
#' where f_X is the pdf of distribution X, with an integration analogue for continuous distributions.
#' The base of the logarithm of the equation determines the type of entropy computed. By default we use base 2 to compute entropy in 'Shannons' or 'bits'.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "entropy", function(base = 2) {
  if(testDiscrete(self)){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = getWorkingSupport(self)
    probs = self$pdf(rng)
    logs = log(self$pdf(rng), base)
    return(-sum(probs * logs))
  } else if(testContinuous(self)){
    warning("Results from numerical integration are approximate only, better results may be available.")
    return(-integrate(function(x) {
      probs = self$pdf(x)
      logs = log(self$pdf(x), base)
      logs[probs==0] = 0
      return(probs * logs)
    }, lower = self$inf(), upper = self$sup())$value)
  }
})

#-------------------------------------------------------------
# Public Methods - Skewness
#-------------------------------------------------------------
#' @title Distribution Skewness
#' @name skewness
#' @description Skewness of a distribution
#'
#' @usage skewness(object)
#' @section R6 Usage: $skewness()
#'
#' @param object Distribution.
#'
#' @details The skewness of a distribution is defined by the third standardised moment of the
#' distribution,
#' \deqn{sk_X = E_X[(x - \mu)^3]/\sigma^3}
#' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
#' \eqn{\sigma} is the standard deviation of the distribution.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})

#-------------------------------------------------------------
# Public Methods - Kurtosis
#-------------------------------------------------------------
#' @title Distribution Kurtosis
#' @name kurtosis
#' @description Kurtosis of a distribution
#'
#' @usage kurtosis(object, excess = TRUE)
#' @section R6 Usage: $kurtosis(excess = TRUE)
#'
#' @param object Distribution.
#' @param excess logical, if TRUE (default) excess Kurtosis returned
#'
#' @details The kurtosis of a distribution is defined by the fourth standardised moment of the
#' distribution,
#' \deqn{k_X = E_X[(x - \mu)^4]/\sigma^4}
#' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' standard deviation of the distribution. Excess Kurtosis is Kurtosis - 3.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis = self$kthmoment(k = 4, type = "standard")
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})

#-------------------------------------------------------------
# Public Methods - Variance
#-------------------------------------------------------------
#' @name var
#' @title Distribution Variance
#' @description The variance of a distribution, either calculated analytically if possible otherwise
#' estimated numerically.
#'
#' @usage var(object)
#' @section R6 Usage: $var()
#'
#' @param object Distribution.
#'
#' @details The variance of a distribution is defined by the formula
#' \deqn{var_X = E[X^2] - E[X]^2}
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#'
#' @seealso \code{\link{CoreStatistics}}, \code{\link{decorate}} and \code{\link{genExp}}.
#'
#' @export
NULL
CoreStatistics$set("public","var",function(){
  return(self$genExp(trafo = function(x) x^2) - self$genExp()^2)
})

#-------------------------------------------------------------
# Public Methods - Kth Moment
#-------------------------------------------------------------
#' @title Kth Moment
#' @name kthmoment
#' @description Kth standardised or central moment of a distribution
#'
#' @usage kthmoment(object, k, type = "central")
#' @section R6 Usage: $kthmoment(k, type = "central")
#'
#' @param object Distribution.
#' @param k the kth moment to calculate
#' @param type one of 'central', 'standard' or 'zero', abbreviations allowed
#'
#'
#' @details The kth central moment of a distribution is defined by
#' \deqn{CM(k)_X = E_X[(x - \mu)^k]}
#' the kth standardised moment of a distribution is defined by
#' \deqn{SM(k)_X = CM(k)/\sigma^k}
#' the kth zeroth moment of a distribution is defined by
#' \deqn{ZM(k)_X = E_X[(x)^k]}
#' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' standard deviation of the distribution.
#'
#' Abbreviations for the type are allowed but if an unfamiliar input is given then the central moment
#' is computed.
#'
#' @export
NULL
CoreStatistics$set("public", "kthmoment", function(k, type = "central"){

  if(testUnivariate(self)){

    if(grepl("^[c,C]", type)) type <- "central"
    else if(grepl("^[s,S]", type)) type <- "standard"
    else if(grepl("^[z,Z]", type)) type <- "zeroth"
    else{
      warning("Type not recognised, central used")
      type <- "central"
    }

    if(type == "central"){
      if(k == 0)
        return(1)
      if(k == 1)
        return(0)
    }

    if(type == "zeroth"){
      return(self$genExp(trafo = function(x) return((x)^k)))
    }

    centralMoment = self$genExp(trafo = function(x) return((x - self$genExp())^k))

    if(type == "central")
      return(centralMoment)
    else if(type == "standard")
      return(centralMoment / self$sd()^k)
  }
})

#-------------------------------------------------------------
# Public Methods - genExp
#-------------------------------------------------------------
#' @title Generalised Expectation of a Distribution
#' @name genExp
#'
#' @usage genExp(object, trafo = NULL)
#' @section R6 Usage: $genExp(trafo = NULL)
#'
#' @param object Distribution.
#' @param trafo transformation for expectation calculation, see details.
#'
#' @description A generalised expectation function for distributions, for arithmetic mean and more complex
#' numeric calculations.
#' @details The expectation of a probability distribution can be numerically calculated in a variety
#' of different ways, some more efficient than others depending on what is available, this function first
#' checks which analytic methods are present before selecting a numeric strategy.
#'
#' If trafo = NULL, then the arithmetic mean is calculated, i.e. the approximation to \eqn{E[X]}. Any
#' transformation must be given as a function, for example \code{trafo = function(x) x^2}
#' (which is the second moment).
#'
#' @seealso \code{\link{mean}}, \code{\link{CoreStatistics}} and \code{\link{decorate}}.
#'
#' @export
NULL
CoreStatistics$set("public","genExp",function(trafo = NULL){
  if(is.null(trafo)){
    trafo = function(x) return(x)
  }
  if(testDiscrete(self)){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = private$.getWorkingSupportRange()
    pdfs = self$pdf(rng)
    xs = trafo(rng)
    xs[pdfs==0] = 0
    return(sum(pdfs * xs))
  } else if(testContinuous(self)){
    message("Results from numerical integration are approximate only, better results may be available.")
    return(suppressMessages(integrate(function(x) {
      pdfs = self$pdf(x)
      xs = trafo(x)
      xs[pdfs==0] = 0
      return(xs * pdfs)
    }, lower = self$inf(), upper = self$sup())$value))
  }
})

#-------------------------------------------------------------
# Public Methods - cov
#-------------------------------------------------------------
#' @title Numeric Covariance a Distribution
#' @name cov
#' @description A numeric calculation for the covariance of a (multivariate) distribution.
#'
#' @usage cov(object)
#' @section R6 Usage: $cov()
#'
#' @param object Distribution.
#'
#' @details If the distribution is univariate then the variance is returned, otherwise the
#' covariance is calculated numerically.
#'
#' @export
NULL
CoreStatistics$set("public","cov",function(){
  if(testUnivariate(self))
    return(self$var())
}) # TO DO

#-------------------------------------------------------------
# Public Methods - cor
#-------------------------------------------------------------
#' @title Numeric Correlation a Distribution
#' @name cor
#' @description A numeric calculation for the correlation of a (multivariate) distribution.
#'
#' @usage cor(object)
#' @section R6 Usage: $cor()
#'
#' @param object Distribution.
#'
#' @details If the distribution is univariate then nothing is returned, otherwise the
#' correlation is calculated numerically.
#'
#' @export
NULL
CoreStatistics$set("public","cor",function(){}) # TO DO

#-------------------------------------------------------------
# Public Methods - mode
#-------------------------------------------------------------
#' @title Mode of a Distribution
#' @name mode
#' @description A numeric search for the mode(s) of a distribution.
#'
#' @usage mode(object, which)
#' @section R6 Usage: $mode(which)
#'
#' @param object Distribution.
#' @param which which mode of the distribution should be returned, default is the first.
#'
#' @details If the distribution has multiple modes, the first is returned by default, similarly if it has
#' one only. Otherwise the index of the mode to return can be given or "all" if all should be returned.
#'
#' @export
NULL
CoreStatistics$set("public","mode",function(which = 1){
  if(which==1){
    if(testDiscrete(self)){
      rng = try(self$inf():self$sup(),silent = T)
      if(inherits(rng,"try-error"))
        rng = self$getWorkingSupport()
      return(rng[which.max(self$pdf(rng))])
    } else if(testContinuous(self))
      return(optimize(self$pdf,c(self$inf(),1e08), maximum = TRUE))
  }
}) # IN PROGRESS

#-------------------------------------------------------------
# Public Methods - mean
#-------------------------------------------------------------
#' @title Distribution Mean
#'
#' @param x Distribution.
#' @param ... Additional arguments.
#'
#' @section R6 Usage: $mean()
#'
#' @description Arithmetic mean for the probability distribution.
#' @details The artihmetic mean of a (discrete) probability distribution X is the expectation
#' \deqn{E_X(X) = \sum p_X(x)*x}
#' with an integration analogue for continuous distributions.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}}, \code{\link{decorate}} and \code{\link{genExp}}.
#'
#' @export
mean.Distribution <- function(x, ...) {}
CoreStatistics$set("public","mean",function(...){
  return(self$genExp())
})
