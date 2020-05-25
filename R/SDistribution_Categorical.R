#' @name Categorical
#' @template SDist
#' @templateVar ClassName Categorical
#' @templateVar DistName Categorical
#' @templateVar uses in classification supervised learning
#' @templateVar params a given support set, \eqn{x_1,...,x_k}, and respective probabilities, \eqn{p_1,...,p_k},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Only the mode, pdf, cdf, quantile and rand are available for this Distribution, all other methods return \code{NaN}. Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the support set and the probabilities from the \code{probs} parameter. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar constructor ..., probs
#' @templateVar arg1 \code{...} \tab ANY \tab elements in the support Set. See details. \cr
#' @templateVar arg2 \code{probs} \tab numeric \tab vector of probabilities. See details. \cr
#' @templateVar constructorDets a series of elements for the support set and \code{probs} determining the probability of each category occurring. The length of the probability list should equal the number of elements. The probability vector is automatically normalised with \deqn{probs = probs/sum(probs)} If no arguments are given, then defaults to one element '1' with probability one.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function.
#'
#' @examples
#' # Note probabilities are automatically normalised
#' x <- Categorical$new("Bapple", "Banana", 2, probs = c(0.2, 0.4, 1))
#'
#' # Only the probabilities can be changed and must the same length as in construction
#' x$setParameterValue(probs = c(0.1, 0.2, 0.7))
#'
#' # d/p/q/r
#' x$pdf(c("Bapple", "Carrot", 1, 2))
#' x$cdf("Banana") # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mode()
#'
#' summary(x)
#' @export
NULL

Categorical <- R6Class("Categorical", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Categorical",
    short_name = "Cat",
    description = "Categorical Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(..., probs, decorators = NULL) {


      if (...length() == 0) {
        probs <- 1
        dots <- 1
        support <- Set$new(1)
      } else {
        dots <- list(...)
        support <- Set$new(...)
      }

      checkmate::assert(length(dots) == length(probs))

      private$.parameters <- getParameterSet(self, probs)
      self$setParameterValue(probs = probs)

      super$initialize(
        decorators = decorators,
        support = support,
        type = Complex$new(),
        symmetry = if (length(unique(self$getParameterValue("probs"))) == 1) "sym" else "asym"
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(NaN)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (which == "all") {
        return(unlist(self$support$elements)[self$getParameterValue("probs") == max(self$getParameterValue("probs"))])
      } else {
        return(unlist(self$support$elements)[self$getParameterValue("probs") == max(self$getParameterValue("probs"))][which])
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(NaN)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      return(NaN)
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = NULL) {
      return(NaN)
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      return(NaN)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      return(NaN)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      return(NaN)
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(t) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if ("probs" %in% names(lst)) lst$probs <- lst$probs / sum(lst$probs)
      checkmate::assert(length(lst$probs) == self$getParameterValue("categories"))
      super$setParameterValue(lst = lst, error = error)

      if (length(unique(self$getParameterValue("probs"))) == 1) {
        private$.properties$symmetry <- "asymmetric"
      } else {
        private$.properties$symmetry <- "symmetric"
      }

      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      pdf <- self$getParameterValue("probs")[self$support$elements %in% x]
      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- cumsum(self$pdf(self$support$elements))[self$support$elements %in% x]
      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p

      cdf <- matrix(self$cdf(self$support$elements), ncol = self$support$length, nrow = length(p), byrow = T)
      return(self$support$elements[apply(cdf >= p, 1, function(x) min(which(x)))])
    },
    .rand = function(n) {
      sample(self$support$elements, n, TRUE, self$getParameterValue("probs"))
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$probs)) lst <- c(lst, list(probs = paramlst$probs))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Cat", ClassName = "Categorical",
    Type = "\u2102", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-"
  )
)
