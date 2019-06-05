#' @title Core Statistical Methods for Distributions
#'
#' @description Added functionality to distribution objects for numerical statistical
#'   methods. Including a generalised expectation function for more complex numerical calculations.
#'
#' @name CoreStatistics
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist} \tab distribution \tab Distribution to decorate. \cr
#' \code{R62S3} \tab logical \tab If TRUE (default), S3 methods are added for decorators in construction.
#' }
#'
#' @section Added Methods:
#' \tabular{lll}{
#' \strong{Method} \tab \strong{Name} \tab \strong{Link} \cr
#' \code{mgf(t)} \tab Moment generating function \tab \code{\link{mgf}} \cr
#' \code{pgf(t)} \tab Probability generating function \tab \code{\link{pgf}} \cr
#' \code{cf(t)} \tab Characteristic function \tab \code{\link{cf}} \cr
#' \code{iqr()} \tab Interquartile Range \tab \code{\link{iqr}} \cr
#' \code{entropy(base = 2)} \tab (Shannon) Entropy \tab \code{\link{entropy}} \cr
#' \code{skewness()} \tab Skewness \tab \code{\link{skewness}} \cr
#' \code{kurtosis(excess = TRUE)} \tab Kurtosis \tab \code{\link{kurtosis}} \cr
#' \code{kthmoment(type = "central")} \tab Kth Moment \tab \code{\link{kthmoment}} \cr
#' \code{genExp(trafo)} \tab Generalised Expectation \tab \code{\link{genExp}} \cr
#' \code{mode(which = 1)} \tab Mode \tab \code{\link{mode}} \cr
#' \code{var()} \tab Variance \tab \code{\link{var.Distribution}} \cr
#' \code{cov()} \tab Covariance \tab \code{\link{cov.Distribution}} \cr
#' \code{cor()} \tab Correlation \tab \code{\link{cor.Distribution}} \cr
#' }
#'
#'
#' @details Decorator objects add functionality to the given Distribution object
#'  by copying methods in the decorator environment to the chosen Distribution environment. Use the
#'  \code{\link{decorate}} function to decorate a Distribution.
#'
#'  All methods in this decorator use numerical approximations and therefore better results may be available
#'  from analytic computations.
#'
#' @seealso \code{\link{DistributionDecorator}}
#'
#' @examples
#' x = Binomial$new()
#' decorate(x, CoreStatistics)
#' x$iqr()
#'
#' @examples
#' x = Binomial$new(decorators = CoreStatistics)
#' x$kthmoment(4)
NULL


#' @export
CoreStatistics <- R6::R6Class("CoreStatistics", inherit = DistributionDecorator)
#'
#' #' @title Moment Generating Function
#' #' @name mgf
#' #' @description Moment generating function of a distribution
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The moment generating function is defined by
#' #' \deqn{mgf_X(t) = E_X[exp(xt)]}
#' #' where X is the distribution and E_X is the expectation of the distribution X.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname mgf
#' #' @param t integer to evaluate moment generating function at.
#' #' @export
#' mgf.Distribution <- function() {}
CoreStatistics$set("public", "mgf", function(t) {
  return(self$genExp(trafo = function(x) {return(exp(x*t))}))
})
#'
#' #' @title Characteristic Function
#' #' @name cf
#' #' @description Characteristic function of a distribution
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The characteristic function is defined by
#' #' \deqn{cf_X(t) = E_X[exp(xti)]}
#' #' where X is the distribution and E_X is the expectation of the distribution X.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname cf
#' #' @param t integer to evaluate characteristic function at
#' #' @export
#' cf.Distribution <- function() {}
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self)){
    return(self$genExp(trafo = function(x) {return(exp(x*t*1i))}))
  }
})
#'
#' #' @title Probability Generating Function
#' #' @name pgf
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @description Probability generating function of a discrete distribution
#' #' @details The probability generating function is defined by
#' #' \deqn{pgf_X(t) = E_X[exp(z^x)]}
#' #' where X is the distribution and E_X is the expectation of the distribution X.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$genExp(trafo = function(x) {return(z^x)})
    return(x)
  }
})
#'
#' #' @title Interquartile Range
#' #' @name iqr
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @description Interquartile range of a distribution
#' #' @details The interquartile range of a distribution is defined by
#' #' \deqn{iqr_X = q(0.75) - q(0.25)}
#' #' where q is the quantile, or inverse distribution function.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname iqr
#' #' @export
#' iqr.Distribution <- function() {}
CoreStatistics$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})
#'
#' #' @title Entropy
#' #' @name entropy
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @description (Information) Entropy of a distribution
#' #'
#' #' @details The entropy of a distribution is defined by
#' #' \deqn{- sum f_X * log(f_X)}
#' #' where f_X is the pdf of distribution X. The base of the logarithm of the equation determines the
#' #' type of entropy computed. By default we use base 2 to compute entropy in 'Shannons' or 'bits'.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname entropy
#' #' @param base base of the entropy logarithm, default = 2 (Shannon entropy)
#' #' @export
#' entropy.Distribution <- function() {}
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
#'
#' #' @title Skewness
#' #' @name skewness
#' #' @description Skewness of a distribution
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The skewness of a distribution is defined by the third standardised moment of the
#' #' distribution,
#' #' \deqn{sk_X = E_X[(x - \mu)^3]/\sigma^3}
#' #' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' #' standard deviation of the distribution.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname skewness
#' #' @export
#' skewness.Distribution <- function() {}
#'
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})
#'
#' #' @title Kurtosis
#' #' @name kurtosis
#' #' @description Kurtosis of a distribution
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The kurtosis of a distribution is defined by the fourth standardised moment of the
#' #' distribution,
#' #' \deqn{k_X = E_X[(x - \mu)^4]/\sigma^4}
#' #' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' #' standard deviation of the distribution. Excess Kurtosis is Kurtosis - 3.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname kurtosis
#' #' @param excess logical, if TRUE (default) excess Kurtosis returned
#' #' @export
#' kurtosis.Distribution <- function() {}
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis = self$kthmoment(k = 4, type = "standard")
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})
#'
#' #' @title Kth Moment
#' #' @name kthmoment
#' #' @description Kth standardised or central moment of a distribution
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The kth central moment of a distribution is defined by
#' #' \deqn{CM(k)_X = E_X[(x - \mu)^k]}
#' #' the kth standardised moment of a distribution is defined by
#' #' \deqn{SM(k)_X = CM(k)/\sigma^k}
#' #' #' the kth zeroth moment of a distribution is defined by
#' #' \deqn{ZM(k)_X = E_X[(x)^k]}
#' #' where E_X is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' #' standard deviation of the distribution.
#' #'
#' #' Abbreviations for the type are allowed but if an unfamiliar input is given then the central moment
#' #' is computed.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname kthmoment
#' #' @param k the kth moment to calculate
#' #' @param type one of 'central', 'standard' or 'zero', abbreviations allowed
#' #' @export
#' kthmoment.Distribution <- function() {}
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
#'
#' #' @title Generalised Expectation of a Distribution
#' #' @name genExp
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @description A generalised expectation function for distributions, for arithmetic mean and more complex
#' #' numeric calculations.
#' #' @details The expectation of a probability distribution can be numerically calculated in a variety
#' #' of different ways, some more efficient than others depending on what is available, this function first
#' #' checks which analytic methods are present before selecting a numeric strategy.
#' #'
#' #' If trafo = NULL, then the arithmetic mean is calculated, i.e. the approximation to \eqn{E[X]}. Any
#' #' transformation must be given as a function, for example \code{trafo = function(x) x^2}
#' #' (which is the second moment).
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname genExp
#' #' @param trafo transformation for expectation calculation, see details.
#' #' @export
#' genExp.Distribution <- function() {}
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
#'
#' #' @title Numeric Variance of a Distribution
#' #' @name var.Distribution
#' #' @description A numeric variance calculation for distributions.
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details The variance of a probability distribution can be numerically calculated via the generalised
#' #' expectation function and the formula
#' #' \deqn{var_X = E[X^2] - E[X]^2}
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
CoreStatistics$set("public","var",function(){
  return(self$genExp(trafo = function(x) x^2) - self$genExp()^2)
})
#'
#' #' @title Numeric Covariance a Distribution
#' #' @name cov.Distribution
#' #' @description A numeric calculation for the covariance of a (multivariate) distribution.
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details If the distribution is univariate then the variance is returned, otherwise the
#' #' covariance is calculated numerically.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
CoreStatistics$set("public","cov",function(){
  if(testUnivariate(self))
    return(self$var())
}) # TO DO
#'
#' #' @title Numeric Correlation a Distribution
#' #' @name cor.Distribution
#' #' @description A numeric calculation for the correlation of a (multivariate) distribution.
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details If the distribution is univariate then nothing is returned, otherwise the
#' #' correlation is calculated numerically.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
CoreStatistics$set("public","cor",function(){}) # TO DO
#'
#' #' @title Mode of a Distribution
#' #' @name mode
#' #' @description A numeric search for the mode(s) of a distribution.
#' #'
#' #' @param object an object used to select a method.
#' #' @param ... further arguments passed to or from other methods.
#' #'
#' #' @details If the distribution has multiple modes, the first is returned by default, similarly if it has
#' #' one only. Otherwise the index of the mode to return can be given or "All" if all should be returned.
#' #'
#' #' Documentation is for the S3 method, the first parameter can be omitted if calling as
#' #' an R6 method. CoreStatistics methods can only be used if the distribution has first been decorated
#' #' with \code{decorate(Distribution, CoreStatistics)}.
#' #'
#' #' @seealso \code{\link{decorate}} for the decorator function and \code{\link{CoreStatistics}} and
#' #' \code{\link{ExoticStatistics}} for other available methods for decorating.
#' #'
#' #' @export
#' NULL
#' #' @rdname mode
#' #' @param which which mode of the distribution should be returned, default is the first.
#' #' @export
#' mode.Distribution <- function() {}
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
