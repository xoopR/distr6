
#' @name Multinomial
#' @template SDist
#' @templateVar ClassName Multinomial
#' @templateVar DistName Multinomial
#' @templateVar uses to extend the binomial distribution to multiple variables, for example to model the rolls of multiple dice multiple times
#' @templateVar params number of trials, \eqn{n}, and probabilities of success, \eqn{p_1,...,p_k},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_1,x_2,\ldots,x_k) = n!/(x_1! * x_2! * \ldots * x_k!) * p_1^{x_1} * p_2^{x_2} * \ldots * p_k^{x_k}}
#' @templateVar paramsupport \eqn{p_i, i = {1,\ldots,k}; \sum p_i = 1} and \eqn{n = {1,2,\ldots}}
#' @templateVar distsupport \eqn{\sum x_i = N}
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
#'
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
#' @family discrete distributions
#' @family multivariate distributions
#'
#' @export
Multinomial <- R6Class("Multinomial",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Multinomial",
    short_name = "Multinom",
    description = "Multinomial Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param size `(integer(1))`\cr
    #' Number of trials, defined on the positive Naturals.
    #' @param probs `(numeric())`\cr
    #' Vector of probabilities. Automatically normalised by
    #' `probs = probs/sum(probs)`.
    initialize = function(size = 10, probs = c(0.5, 0.5), decorators = NULL) {

      if (length(probs) == 1) stop("Length of probs is '1', use Binomial distribution instead.")

      private$.parameters <- getParameterSet(self, size, probs)
      self$setParameterValue(size = size, probs = probs)

      private$.variates <- length(probs)

      super$initialize(
        decorators = decorators,
        support = setpower(Set$new(0:size, class = "integer"), length(probs)),
        type = setpower(Naturals$new(), length(probs))
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(self$getParameterValue("size") * self$getParameterValue("probs"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      cov <- self$getParameterValue("probs") %*% t(self$getParameterValue("probs")) * -self$getParameterValue("size")
      diag(cov) <- self$getParameterValue("size") * self$getParameterValue("probs") * (1 - self$getParameterValue("probs"))
      return(cov)
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
    kurtosis = function(excess = TRUE) {
      return(NaN)
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      size <- self$getParameterValue("size")
      probs <- self$getParameterValue("probs")
      K <- self$getParameterValue("K")

      s1 <- -log(factorial(size), base)
      s2 <- -size * sum(probs * log(probs, base))
      s3 <- 0
      for (i in 1:K) {
        for (j in 0:size) {
          s3 <- s3 + (choose(size, j) * (probs[[i]]^j) * ((1 - probs[[i]])^(size - j)) * (log(factorial(j), base)))
        }
      }

      return(s1 + s2 + s3)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      checkmate::assert(length(t) == self$getParameterValue("K"))
      return(sum(exp(t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      checkmate::assert(length(t) == self$getParameterValue("K"))
      return(sum(exp(1i * t) * self$getParameterValue("probs"))^self$getParameterValue("size"))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      checkmate::assert(length(z) == self$getParameterValue("K"))
      return(sum(self$getParameterValue("probs") * z)^self$getParameterValue("size"))
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }

      lst$probs <- lst$probs / sum(lst$probs)
      lst$K <- NULL

      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {

      checkmate::assertMatrix(x, ncols = length(self$getParameterValue("probs")))

      if (checkmate::testList(self$getParameterValue("probs"))) {
        mapply(extraDistr::dmnom,
          size = self$getParameterValue("size"),
          prob = self$getParameterValue("probs"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dmnom(x,
          size = self$getParameterValue("size"),
          prob = self$getParameterValue("probs"),
          log = log
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("probs"))) {
        mapply(extraDistr::rmnom,
          size = self$getParameterValue("size"),
          prob = self$getParameterValue("probs"),
          MoreArgs = list(n = n),
          SIMPLIFY = FALSE
        )
      } else {
        extraDistr::rmnom(n,
          size = self$getParameterValue("size"),
          prob = self$getParameterValue("probs")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "multivariate"),

    .isCdf = FALSE,
    .isQuantile = FALSE
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Multinom", ClassName = "Multinomial",
    Type = "\u21150^K", ValueSupport = "discrete",
    VariateForm = "multivariate",
    Package = "extraDistr"
  )
)
