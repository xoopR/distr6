# nolint start
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
# nolint end
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
    #' @param ... Unused.
    mean = function(...) {
      size <- self$getParameterValue("size")
      probs <- self$getParameterValue("probs")

      if (checkmate::testList(probs)) {
        return(t(mapply(
          function(s, p) s * p,
          size,
          probs
        )))
      } else {
        return(size * probs)
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      size <- self$getParameterValue("size")
      probs <- self$getParameterValue("probs")

      if (checkmate::testList(probs)) {
        covar <- array(dim = c(length(probs[[1]]), length(probs[[1]]), length(probs)))
        for (i in seq_along(size)) {
          covar[, , i] <- probs[[i]] %*% t(probs[[i]]) * -size[[i]]
          diag(covar[, , i]) <- size[[i]] * probs[[i]] * (1 - probs[[i]])
        }
        return(covar)
      } else {
        cov <- probs %*% t(probs) * -size
        diag(cov) <- size * probs * (1 - probs)
        return(cov)
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      rep(NaN, length(self$getParameterValue("size")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      rep(NaN, length(self$getParameterValue("size")))
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      size <- self$getParameterValue("size")
      probs <- self$getParameterValue("probs")

      if (checkmate::testList(size)) {
        K <- length(probs[[1]])
        ent <- c()
        for (k in seq_along(size)) {
          s1 <- -log(factorial(size[[k]]), base)
          s2 <- -size[[k]] * sum(probs[[k]] * log(probs[[k]], base))
          s3 <- 0
          for (i in 1:K) {
            for (j in 0:size[[k]]) {
              s3 <- s3 + (choose(size[[k]], j) * (probs[[k]][[i]]^j) * # nolint
                            ((1 - probs[[k]][[i]])^(size[[k]] - j)) * (log(factorial(j), base))) # nolint
            }
          }
          ent <- c(ent, s1 + s2 + s3)
        }
      } else {
        K <- length(probs)
        s1 <- -log(factorial(size), base)
        s2 <- -size * sum(probs * log(probs, base))
        s3 <- 0
        for (i in 1:K) {
          for (j in 0:size) {
            s3 <- s3 + (choose(size, j) * (probs[[i]]^j) *
                          ((1 - probs[[i]])^(size - j)) * (log(factorial(j), base))) # nolint
          }
        }
        ent <- s1 + s2 + s3
      }



      return(ent)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      probs <- self$getParameterValue("probs")
      checkmate::assert(length(t) == length(probs))
      return(sum(exp(t) * probs)^self$getParameterValue("size"))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      probs <- self$getParameterValue("probs")
      checkmate::assert(length(t) == length(probs))
      return(sum(exp(1i * t) * probs)^self$getParameterValue("size"))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      probs <- self$getParameterValue("probs")
      checkmate::assert(length(z) == length(probs))
      return(sum(probs * z)^self$getParameterValue("size"))
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {

      probs <- self$getParameterValue("probs")

      if (checkmate::testList(probs)) {
        checkmate::assertMatrix(x, ncols = length(probs[[1]]))
        mapply(extraDistr::dmnom,
          size = self$getParameterValue("size"),
          prob = probs,
          MoreArgs = list(x = x, log = log)
        )
      } else {
        checkmate::assertMatrix(x, ncols = length(probs))
        extraDistr::dmnom(x,
          size = self$getParameterValue("size"),
          prob = probs,
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
    Package = "extraDistr", Tags = "limits"
  )
)
