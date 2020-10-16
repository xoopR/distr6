# nolint start
#' @name Dirichlet
#' @template SDist
#' @templateVar ClassName Dirichlet
#' @templateVar DistName Dirichlet
#' @templateVar uses as a prior in Bayesian modelling and is multivariate generalisation of the Beta distribution
#' @templateVar params concentration parameters, \eqn{\alpha_1,...,\alpha_k},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x_1,...,x_k) = (\prod \Gamma(\alpha_i))/(\Gamma(\sum \alpha_i))\prod(x_i^{\alpha_i - 1})}
#' @templateVar paramsupport \eqn{\alpha = \alpha_1,...,\alpha_k; \alpha > 0}, where \eqn{\Gamma} is the gamma function
#' @templateVar distsupport \eqn{x_i \ \epsilon \ (0,1), \sum x_i = 1}{x_i \epsilon (0,1), \sum x_i = 1}
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
# nolint end
#' @details
#' Sampling is performed via sampling independent Gamma distributions and normalising the samples
#' (Devroye, 1986).
#'
#' @references
#' Devroye, Luc (1986).
#' Non-Uniform Random Variate Generation.
#' Springer-Verlag. ISBN 0-387-96305-7.
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
#' @examples
#' d <- Dirichlet$new(params = c(2, 5, 6))
#' d$pdf(0.1, 0.4, 0.5)
#' d$pdf(c(0.3, 0.2), c(0.6, 0.9), c(0.9, 0.1))
#' @family continuous distributions
#' @family multivariate distributions
#'
#' @export
Dirichlet <- R6Class("Dirichlet",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Dirichlet",
    short_name = "Diri",
    description = "Dirichlet Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param params `numeric()`\cr
    #' Vector of concentration parameters of the distribution defined on the positive Reals.
    initialize = function(params = c(1, 1), decorators = NULL) {

      private$.parameters <- getParameterSet(self, params)
      self$setParameterValue(params = params)

      private$.variates <- length(params)

      super$initialize(
        decorators = decorators,
        support = setpower(Interval$new(0, 1, type = "()"), length(params)),
        type = setpower(Interval$new(0, 1, type = "()"), length(params))
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      params <- self$getParameterValue("params")
      if (checkmate::testList(params)) {
        return(t(sapply(params, function(x) x / sum(x))))
      } else {
        return(params / sum(params))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      params <- self$getParameterValue("params")

      if (checkmate::testList(params)) {
        mode <- matrix(NaN, ncol = length(params[[1]]), nrow = length(params))
        for (i in seq_along(params)) {
          pari <- params[[i]]
          mode[i, pari > 1] <- (pari[pari > 1] - 1) / (sum(pari) - length(pari))
        }
        return(mode)
      } else {
        mode <- rep(NaN, length(params))
        mode[params > 1] <- (params[params > 1] - 1) / (sum(params) - length(params))
        return(mode)
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      params <- self$getParameterValue("params")

      if (checkmate::testList(params)) {
        K <- length(params[[1]])
        covar <- array(dim = c(K, K, length(params)))
        for (i in seq_along(params)) {
          parami <- params[[i]] / sum(params[[i]])
          var <- (parami * (1 - parami)) / (sum(params[[i]]) + 1)
          covar[, , i] <- matrix((-parami %*% t(parami)) /
            (sum(params[[i]]) + 1), nrow = K, ncol = K)
          diag(covar[, , i]) <- var
        }
        return(covar)
      } else {
        K <- length(params)
        parami <- params / sum(params)
        var <- (parami * (1 - parami)) / (sum(params) + 1)
        covar <- matrix((-parami %*% t(parami)) / (sum(params) + 1), nrow = K, ncol = K)
        diag(covar) <- var
        return(covar)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      params <- self$getParameterValue("params")

      if (checkmate::testList(params)) {
        sapply(params, function(x) {
          log(prod(gamma(x)) / gamma(sum(x)), 2) + (sum(x) - length(x)) * digamma(sum(x)) -
            sum((x - 1) * digamma(x))
        })
      } else {
        return(log(prod(gamma(params)) / gamma(sum(params)), 2) + (sum(params) - length(params))
        * digamma(sum(params)) - sum((params - 1) * digamma(params)))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      params <- self$getParameterValue("params")

      if (checkmate::testList(params)) {
        checkmate::assertMatrix(x, ncols = length(params[[1]]))
        mapply(extraDistr::ddirichlet,
          alpha = params,
          MoreArgs = list(x = x, log = log)
        )
      } else {
        checkmate::assertMatrix(x, ncols = length(params))
        extraDistr::ddirichlet(x,
          alpha = params,
          log = log
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("params"))) {
        mapply(extraDistr::rdirichlet,
          alpha = self$getParameterValue("params"),
          MoreArgs = list(n = n),
          SIMPLIFY = FALSE
        )
      } else {
        extraDistr::rdirichlet(n,
          alpha = self$getParameterValue("params")
        )
      }
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "multivariate"),

    .isCdf = FALSE,
    .isQuantile = FALSE
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Diri", ClassName = "Dirichlet",
    Type = "[0,1]^K", ValueSupport = "continuous",
    VariateForm = "multivariate",
    Package = "extraDistr", Tags = ""
  )
)
