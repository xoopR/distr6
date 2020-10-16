# nolint start
#' @name Triangular
#' @template SDist
#' @aliases SymmetricTriangular
#' @templateVar ClassName Triangular
#' @templateVar DistName Triangular
#' @templateVar uses to model population data where only the minimum, mode and maximum are known (or can be reliably estimated), also to model the sum of standard uniform distributions
#' @templateVar params lower limit, \eqn{a}, upper limit, \eqn{b}, and mode, \eqn{c},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \cr\cr \eqn{f(x) = 0, x < a} \cr \eqn{f(x) = 2(x-a)/((b-a)(c-a)), a \le x < c} \cr \eqn{f(x) = 2/(b-a), x = c} \cr \eqn{f(x) = 2(b-x)/((b-a)(b-c)), c < x \le b} \cr \eqn{f(x) = 0, x > b}
#' @templateVar paramsupport \eqn{a,b,c \ \in \ R}{a,b,c \epsilon R}, \eqn{a \le c \le b}
#' @templateVar distsupport \eqn{[a, b]}
# nolint end
#'
#' @template param_lower
#' @template param_upper
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
Triangular <- R6Class("Triangular",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Triangular",
    short_name = "Tri",
    description = "Triangular Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mode `(numeric(1))`\cr
    #' Mode of the distribution, if `symmetric = TRUE` then determined automatically.
    #' @param symmetric `(logical(1))`\cr
    #' If `TRUE` then the symmetric Triangular distribution is constructed, where the `mode` is
    #' automatically calculated. Otherwise `mode` can be set manually. Cannot be changed after
    #' construction.
    #'
    #' @examples
    #' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)
    #' Triangular$new(lower = 2, upper = 5, symmetric = FALSE)
    #' Triangular$new(lower = 2, upper = 5, mode = 4)
    #'
    #' # You can view the type of Triangular distribution with $description
    #' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)$description
    #' Triangular$new(lower = 2, upper = 5, symmetric = FALSE)$description
    initialize = function(lower = 0, upper = 1, mode = (lower + upper) / 2,
                          symmetric = FALSE,
                          decorators = NULL) {


      if (symmetric) {
        mode <- NULL
        self$description <- "Symmetric Triangular Probability Distribution."
        symmetry <- "symmetric"
      } else {
        self$description <- "Triangular Probability Distribution."
        private$.type <- "asymmetric"

        if (mode == (lower + upper) / 2) {
          symmetry <- "symmetric"
        } else {
          symmetry <- "asymmetric"
        }
      }

      private$.parameters <- getParameterSet(self, lower, upper, mode, symmetric)
      self$setParameterValue(lower = lower, upper = upper, mode = mode)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper),
        symmetry = symmetry,
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      (unlist(self$getParameterValue("lower")) + unlist(self$getParameterValue("upper")) +
        unlist(self$getParameterValue("mode"))) / 3
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      unlist(self$getParameterValue("mode"))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    median = function() {
      lower <- unlist(self$getParameterValue("lower"))
      upper <- unlist(self$getParameterValue("upper"))
      mode <- unlist(self$getParameterValue("mode"))
      median <- numeric(length(lower))
      ind <- mode >= (lower + upper) / 2
      median[ind] <- lower[ind] + sqrt((upper[ind] - lower[ind]) * (mode[ind] - lower[ind])) /
        sqrt(2)
      median[!ind] <- upper[!ind] - sqrt((upper[!ind] - lower[!ind]) * (upper[!ind] - mode[!ind])) /
        sqrt(2)
      return(median)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      lower <- unlist(self$getParameterValue("lower"))
      upper <- unlist(self$getParameterValue("upper"))
      mode <- unlist(self$getParameterValue("mode"))

      return((lower^2 + upper^2 + mode^2 - lower * upper - lower * mode - upper * mode) / 18)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      lower <- unlist(self$getParameterValue("lower"))
      upper <- unlist(self$getParameterValue("upper"))
      mode <- unlist(self$getParameterValue("mode"))

      num <- sqrt(2) * (lower + upper - 2 * mode) * (2 * lower - upper - mode) *
        (lower - 2 * upper + mode)
      den <- 5 * (lower^2 + upper^2 + mode^2 - lower * upper - lower * mode - upper * mode)^1.5
      return(num / den)
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      if (excess) {
        return(rep(-0.6, length(self$getParameterValue("lower"))))
      } else {
        return(rep(2.4, length(self$getParameterValue("lower"))))
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      0.5 * log((unlist(self$getParameterValue("upper")) -
        unlist(self$getParameterValue("lower"))) / 2, base)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")

      num <- 2 * ((upper - mode) * exp(lower * t) - (upper - lower) * exp(mode * t) + (mode - lower)
      * exp(upper * t))
      den <- (upper - lower) * (mode - lower) * (upper - mode) * t^2

      return(num / den)
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")

      num <- -2 * ((upper - mode) * exp(1i * lower * t) - (upper - lower) * exp(1i * mode * t) +
        (mode - lower) * exp(1i * upper * t))
      den <- (upper - lower) * (mode - lower) * (upper - mode) * t^2

      return(num / den)
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      private$.properties$support <- Interval$new(lower, upper)
      if (private$.type != "symmetric") {
        if (self$getParameterValue("mode") == (lower + upper) / 2) {
          private$.properties$symmetry <- "symmetric"
        } else {
          private$.properties$symmetry <- "asymmetric"
        }
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::dtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dtriang(
          x,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::ptriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::ptriang(
          x,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::qtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qtriang(
          p,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::rtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rtriang(
          n,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),

    .type = "symmetric"
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Tri", ClassName = "Triangular",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr", Tags = "limits"
  )
)
