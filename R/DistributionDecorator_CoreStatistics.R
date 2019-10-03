#' @name CoreStatistics
#'
#' @title Core Statistical Methods for Distributions
#'
#' @description This decorator adds numeric methods for missing analytic expression in distr6 Distribution
#' objects as well as adding generalised expectation and moments functions.
#'
#' @details Decorator objects add functionality to the given Distribution object by copying methods
#' in the decorator environment to the chosen Distribution environment. See the 'Added Methods' section
#' below to find details of the methods that are added to the Distribution. Methods already
#' present in the distribution are not overwritten by the decorator.
#'
#' Use \code{\link{decorate}} to decorate a Distribution.
#'
#' All methods in this decorator use numerical approximations and therefore better results may be available
#' from analytic computations.
#'
#' @section Constructor: CoreStatistics$new(distribution)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distribution} \tab distribution \tab Distribution to decorate. \cr
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
#' \code{mode(which = "all")} \tab Mode \tab \code{\link{mode}} \cr
#' \code{variance()} \tab Variance \tab \code{\link{variance}} \cr
#' \code{mean()} \tab Arithmetic mean \tab \code{\link{mean.Distribution}} \cr
#' }
#'
#' @seealso \code{\link{decorate}}, \code{\link{listDecorators}}
#'
#' @return Returns a decorated R6 object inheriting from class SDistribution with the methods listed below
#' added to the SDistribution methods.
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
.distr6$decorators <- append(.distr6$decorators, list(CoreStatistics = CoreStatistics))
#-------------------------------------------------------------
# mgf
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
#' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return Moment generating function evaluated at t as a numeric.
#'
#' @export
NULL
CoreStatistics$set("public", "mgf", function(t) {
  return(self$genExp(trafo = function(x) {return(exp(x*t))}))
})

#-------------------------------------------------------------
# cf
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
#' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return Characteristic function evaluated at t as a numeric.
#'
#' @export
NULL
CoreStatistics$set("public", "cf", function(t) {
  if(testDiscrete(self))
    return(self$genExp(trafo = function(x) {return(exp(x*t*1i))}))
  else
    return(self$genExp(trafo = function(x) {return(Re(exp(x*t*1i)))}) +
             1i * self$genExp(trafo = function(x) {return(Im(exp(x*t*1i)))}))
})

#-------------------------------------------------------------
# pgf
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
#' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return Probability generating function evaluated at z as a numeric if distribution is discrete,
#' otherwise NaN.
#'
#' @export
NULL
CoreStatistics$set("public", "pgf", function(z) {
  if(testDiscrete(self)){
    x = self$genExp(trafo = function(x) {return(z^x)})
    return(x)
  } else
    return(NaN)
})

#-------------------------------------------------------------
# entropy
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
#' \deqn{- \sum (f_X)log(f_X)}
#' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for continuous distributions.
#' The base of the logarithm of the equation determines the type of entropy computed. By default we use
#' base 2 to compute entropy in 'Shannons' or 'bits'.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return Entropy with given base as a numeric.
#'
#' @export
NULL
CoreStatistics$set("public", "entropy", function(base = 2) {
  message(.distr6$message_numeric)
  return(suppressMessages(self$genExp(trafo = function(x) -log(self$pdf(x), base))))
})

#-------------------------------------------------------------
# skewness
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
#' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
#' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
#' \eqn{\sigma} is the standard deviation of the distribution.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return Skewness as a numeric.
#'
#' @export
NULL
CoreStatistics$set("public", "skewness", function() {
  return(self$kthmoment(k = 3, type = "standard"))
})

#-------------------------------------------------------------
# kurtosis
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
#' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
#' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' standard deviation of the distribution. Excess Kurtosis is Kurtosis - 3.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @return Kurtosis as a numeric.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @export
NULL
CoreStatistics$set("public", "kurtosis", function(excess = TRUE) {
  kurtosis =  suppressMessages(self$kthmoment(k = 4, type = "standard"))
  if(testContinuous(self))
    message(.distr6$message_numeric)
  if(excess)
    return(kurtosis - 3)
  else
    return(kurtosis)
})

#-------------------------------------------------------------
# variance
#-------------------------------------------------------------
#' @name variance
#' @title Distribution Variance
#' @description The variance or covariance of a distribution, either calculated analytically if
#' or estimated numerically.
#'
#' @usage variance(object)
#' @section R6 Usage: $variance()
#'
#' @param object Distribution.
#'
#' @details The variance of a distribution is defined by the formula
#' \deqn{var_X = E[X^2] - E[X]^2}
#' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
#' covariance matrix is returned.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @return Variance as a numeric.
#'
#' @seealso \code{\link{CoreStatistics}}, \code{\link{decorate}} and \code{\link{genExp}}.
#'
#' @export
NULL
CoreStatistics$set("public","variance",function(){
  if(testUnivariate(self)){
      message(.distr6$message_numeric)
    return(suppressMessages(self$genExp(trafo = function(x) x^2) - self$genExp()^2))
  }
})

#-------------------------------------------------------------
# kthmoment
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
#' @param type one of 'central', 'standard' or 'raw', abbreviations allowed
#'
#'
#' @details The kth central moment of a distribution is defined by
#' \deqn{CM(k)_X = E_X[(x - \mu)^k]}
#' the kth standardised moment of a distribution is defined by
#' \deqn{SM(k)_X = \frac{CM(k)}{\sigma^k}}{SM(k)_X = CM(k)/\sigma^k}
#' the kth raw moment of a distribution is defined by
#' \deqn{RM(k)_X = E_X[x^k]}
#' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and \eqn{\sigma} is the
#' standard deviation of the distribution.
#'
#' Abbreviations for the type are allowed but if an unfamiliar input is given then the central moment
#' is computed.
#'
#' Can only be used after decorating with \code{\link{CoreStatistics}}.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}
#'
#' @return If univariate, the given k-moment as a numeric, otherwise NULL.
#'
#' @export
NULL
CoreStatistics$set("public", "kthmoment", function(k, type = "central"){

  if(testUnivariate(self)){

    if(grepl("^[c,C]", type)) type <- "central"
    else if(grepl("^[s,S]", type)) type <- "standard"
    else if(grepl("^[r,R]", type)) type <- "raw"
    else{
      message("Type not recognised, central used")
      type <- "central"
    }

    if(type == "central"){
      if(k == 0)
        return(1)
      if(k == 1)
        return(0)
    }

    message(.distr6$message_numeric)

    if(type == "raw"){
      suppressMessages(return(self$genExp(trafo = function(x) return(x^k))))
    }

    centralMoment = suppressMessages(self$genExp(trafo = function(x) return((x - self$genExp())^k)))

    if(type == "central")
      return(centralMoment)
    else if(type == "standard")
      suppressMessages(return(centralMoment / self$stdev()^k))
  }
})

#-------------------------------------------------------------
# genExp
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
#' Can only be used after decorating with \code{\link{CoreStatistics}}.
#'
#' @seealso \code{\link{mean}}, \code{\link{CoreStatistics}} and \code{\link{decorate}}.
#'
#' @return The given expectation as a numeric, otherwise NULL.
#'
#' @export
NULL
CoreStatistics$set("public","genExp",function(trafo = NULL){
  if(is.null(trafo)){
    trafo = function() return(x)
    formals(trafo) = alist(x = )
  }

  if(self$support()$class() == "integer"){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error")){
      lower = ifelse(self$inf() == -Inf, -1e03, self$inf())
      upper = ifelse(self$sup() == Inf, 1e03, self$sup())
      rng = lower:upper
    }
    pdfs = self$pdf(rng)
    xs = trafo(rng)
    xs[pdfs==0] = 0
    return(sum(pdfs * xs))
  } else {
    message(.distr6$message_numeric)
    return(suppressMessages(integrate(function(x) {
      pdfs = self$pdf(x)
      xs = trafo(x)
      xs[pdfs==0] = 0
      return(xs * pdfs)
    }, lower = self$inf(), upper = self$sup())$value))
  }
})
#-------------------------------------------------------------
# mode
#-------------------------------------------------------------
#' @title Mode of a Distribution
#' @name mode
#' @description A numeric search for the mode(s) of a distribution.
#'
#' @usage mode(object, which = "all")
#' @section R6 Usage: $mode(which = "all")
#'
#' @param object Distribution.
#' @param which which mode of the distribution should be returned, default is all.
#'
#' @details If the distribution has multiple modes, all are returned by default. Otherwise the index
#' of the mode to return can be given or "all" if all should be returned.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{decorate}}.
#'
#' @return The estimated mode as a numeric, either all modes (if multiple) or the ordered mode given in \code{which}.
#'
#' @export
NULL
CoreStatistics$set("public","mode",function(which = "all"){
  if(private$.isRand)
    return(modal(round(self$rand(1e5),4)))
  else{
    lower <- ifelse(self$inf() == -Inf, -1e3, self$inf())
    upper <- ifelse(self$sup() == Inf, 1e3, self$sup())

    if(testDiscrete(self))
      return((self$inf():self$sup())[which.max(self$pdf(self$inf():self$sup()))])
    else
      return(optimize(self$pdf, interval=c(lower, upper), maximum = T)$maximum)
  }
})

#-------------------------------------------------------------
# mean
#-------------------------------------------------------------
#' @title Distribution Mean
#'
#' @param x Distribution.
#' @param ... Additional arguments.
#'
#' @section R6 Usage: $mean()
#'
#' @description Arithmetic mean for the probability distribution.
#' @details The arithmetic mean of a (discrete) probability distribution X is the expectation
#' \deqn{E_X(X) = \sum p_X(x)*x}
#' with an integration analogue for continuous distributions.
#'
#' If an analytic expression isn't available, returns error. To impute a numerical expression, use the
#' \code{\link{CoreStatistics}} decorator.
#'
#' @seealso \code{\link{CoreStatistics}}, \code{\link{decorate}} and \code{\link{genExp}}.
#'
#' @return Mean as a numeric.
#'
#' @export
mean.Distribution <- function(x, ...) {}
CoreStatistics$set("public","mean",function(...){
  return(self$genExp())
})
