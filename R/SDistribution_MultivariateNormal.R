
#' @name MultivariateNormal
#' @template SDist
#' @templateVar ClassName MultivariateNormal
#' @templateVar DistName Multivariate Normal
#' @templateVar uses to generalise the Normal distribution to higher dimensions, and is commonly associated with Gaussian Processes
#' @templateVar params mean, \eqn{\mu}, and covariance matrix, \eqn{\Sigma},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x_1,...,x_k) = (2 * \pi)^{-k/2}det(\Sigma)^{-1/2}exp(-1/2(x-\mu)^T\Sigma^{-1}(x-\mu))}
#' @templateVar paramsupport \eqn{\mu \epsilon R^{k}} and \eqn{\Sigma \epsilon R^{k x k}}
#' @templateVar distsupport the Reals and only when the covariance matrix is positive-definite
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
#' @details
#' Sampling is performed via the Cholesky decomposition using [chol].
#' @references
#' Gentle, J.E. (2009).
#' Computational Statistics.
#' Statistics and Computing. New York: Springer. pp. 315–316.
#' doi:10.1007/978-0-387-98144-4. ISBN 978-0-387-98143-7.
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#'
#' @family continuous distributions
#' @family multivariate distributions
#'
#' @export
MultivariateNormal <- R6Class("MultivariateNormal", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "MultivariateNormal",
    short_name = "MultiNorm",
    description = "Multivariate Normal Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param mean `(numeric())`\cr
    #' Vector of means, defined on the Reals.
    #' @param cov `(matrix()|vector())` \cr
    #' Covariance of the distribution, either given as a matrix or vector coerced to a
    #' matrix via `matrix(cov, nrow = K, byrow = FALSE)`. Must be semi-definite.
    #' @param prec `(matrix()|vector())` \cr
    #' Precision of the distribution, inverse of the covariance matrix. If supplied then `cov` is
    #' ignored. Given as a matrix or vector coerced to a
    #' matrix via `matrix(cov, nrow = K, byrow = FALSE)`. Must be semi-definite.
    initialize = function(mean = rep(0, 2), cov = c(1, 0, 0, 1),
                          prec = NULL, decorators = NULL) {

      if (length(mean) == 1) stop("Length of mean is '1', use Normal distribution instead.")

      private$.parameters <- getParameterSet(self, mean, cov, prec)
      self$setParameterValue(mean = mean, cov = cov, prec = prec)

      private$.variates = length(mean)

      super$initialize(
        decorators = decorators,
        support = setpower(Reals$new(), length(mean)),
        type = setpower(Reals$new(), length(mean))
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = 'all') {
      return(self$getParameterValue("mean"))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(self$getParameterValue("cov"))
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      return(0.5 * log(det(2 * pi * exp(1) * self$getParameterValue("cov")), base))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      mean <- self$getParameterValue("mean")
      checkmate::assert(length(t) == length(mean))
      return(exp((mean %*% t(t(t))) + (0.5 * t %*% self$getParameterValue("cov") %*% t(t(t)))))
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      mean <- self$getParameterValue("mean")
      checkmate::assert(length(t) == length(mean))
      return(exp((1i * mean %*% t(t(t))) + (0.5 * t %*% self$getParameterValue("cov") %*% t(t(t)))))
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      K <- length(self$getParameterValue("mean"))
      if (is.null(lst)) {
        lst <- list(...)
      }
      if (!is.null(lst$cov)) {
        if (any(dim(lst$cov) != c(K, K)) |
            length(lst$cov) != K^2) {
          lst$cov <- suppressWarnings(matrix(lst$cov, nrow = K, ncol = K))
        }
        lst$cov <- as.numeric(lst$cov)
      }
      if (!is.null(lst$prec)) {
        if (any(dim(lst$prec) != c(K, K)) |
            length(lst$prec) != K^2) {
          lst$prec <- suppressWarnings(matrix(lst$prec, nrow = K, ncol = K))
        }
        lst$prec <- as.numeric(lst$prec)
      }
      if (!is.null(lst$mean)) {
        lst$mean <- as.numeric(lst$mean)
        if (length(lst$mean) < K) {
          lst$mean <- rep(lst$mean, K)
        }
        if (length(lst$mean) > K) {
          lst$mean <- lst$mean[1:K]
        }
        lst$mean <- as.numeric(lst$mean)
      }

      super$setParameterValue(lst = lst, error = error)
      invisible(self)
    },

    #' @description
    #' Returns the value of the supplied parameter.
    #' @param id `character()` \cr
    #' id of parameter support to return.
    getParameterValue = function(id, error = "warn") {
      if ("cov" %in% id) {
        return(matrix(super$getParameterValue("cov", error),
                      nrow = length(super$getParameterValue("mean", error))))
      } else if ("prec" %in% id) {
        return(matrix(super$getParameterValue("prec", error),
                      nrow = length(super$getParameterValue("mean", error))))
      } else {
        return(super$getParameterValue(id, error))
      }
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {

      cov <- self$getParameterValue("cov")
      mean <- self$getParameterValue("mean")

      dmvn <- function(x, cov, mean, log) {
        K <- length(mean)

        if (checkmate::testNumeric(cov)) {
          cov <- matrix(cov, nrow = K)
        }

        if (isSymmetric.matrix(cov) & all(eigen(cov, only.values = TRUE)$values > 0)) {

          checkmate::testMatrix(x, ncols = K)
          mean <- matrix(mean, nrow = nrow(x), ncol = K, byrow = TRUE)

          if (log) {
            return(as.numeric(-((K / 2) * log(2 * pi)) - (log(det(cov)) / 2) -
                                (diag((x - mean) %*% solve(cov) %*% t(x - mean)) / 2)))
          } else {
            return(as.numeric((2 * pi)^(-K / 2) * det(cov)^-0.5 *
                                exp(-0.5 * diag((x - mean) %*% solve(cov) %*% t(x - mean)))))
          }

        } else {
          return(NaN)
        }
      }

      if (checkmate::testList(cov)) {
        mapply(dmvn,
               cov = cov,
               mean = mean,
               MoreArgs = list(x = x, log = log)
        )
      } else {
        dmvn(x, cov = cov, mean = mean, log = log)
      }
    },
    .rand = function(n) {

      cov <- self$getParameterValue("cov")
      mean <- self$getParameterValue("mean")

      rmvn <- function(n, cov, mean) {
        K <- length(mean)

        if (checkmate::testNumeric(cov)) {
          cov <- matrix(cov, nrow = K)
        }

        xs <- matrix(rnorm(K * n), ncol = n)

        return(data.table::data.table(t(mean + t(chol(cov)) %*% xs)))
      }

      if (checkmate::testList(cov)) {
        mapply(rmvn,
               cov = cov,
               mean = mean,
               MoreArgs = list(n = n),
               SIMPLIFY = FALSE
        )
      } else {
        rmvn(n, cov = cov, mean = mean)
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$mean)) lst <- c(lst, list(mean = paramlst$mean))
      if (!is.null(paramlst$cov)) lst <- c(lst, list(cov = paramlst$cov))
      if (!is.null(paramlst$prec)) {
        K <- length(self$getParameterValue("mean"))
        lst <- c(lst, list(cov = solve(matrix(paramlst$prec,
                                              nrow = K,
                                              ncol = K
        ))))
      }
      return(lst)
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
    ShortName = "MultiNorm", ClassName = "MultivariateNormal",
    Type = "\u211D^K", ValueSupport = "continuous",
    VariateForm = "multivariate",
    Package = "-"
  )
)
