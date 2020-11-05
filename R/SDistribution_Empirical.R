
#' @name Empirical
#' @template SDist
#' @templateVar ClassName Empirical
#' @templateVar DistName Empirical
#' @templateVar uses in sampling such as MCMC
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{p(x) = \sum I(x = x_i) / k}
#' @templateVar paramsupport \eqn{x_i \epsilon R, i = 1,...,k}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @details
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the support set and uniform probabilities. Sampling is performed with replacement, which is
#' consistent with other distributions but non-standard for Empirical distributions. Use
#' [simulateEmpiricalDistribution] to sample without replacement.
#'
#' The cdf and quantile assumes that the
#' elements are supplied in an indexed order (otherwise the results are meaningless).
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
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
Empirical <- R6Class("Empirical",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Empirical",
    short_name = "Emp",
    description = "Empirical Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param samples `(numeric())` \cr
    #' Vector of observed samples, see examples.
    #' @examples
    #' Empirical$new(runif(1000))
    initialize = function(samples = 1, decorators = NULL) {

      samples <- sort(as.numeric(samples))

      data <- data.table::as.data.table(table(samples))
      data$samples <- as.numeric(data$samples)
      data <- cbind(data, cumN = cumsum(data$N))

      private$.parameters <- ParameterSet$new(
        id = "data",
        value = list(data),
        support = Universal$new(),
        settable = FALSE,
        description = "Data"
      )

      super$initialize(
        decorators = decorators,
        support = Tuple$new(samples, universe = Reals$new(), class = "numeric"),
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
      return(mean(self$getParameterValue("data")$samples))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (which == "all") {
        return(modal(self$getParameterValue("data")$samples))
      } else {
        return(modal(self$getParameterValue("data")$samples)[which])
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      data <- self$getParameterValue("data")$samples
      return(sum((data - self$mean())^2) / length(data))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      data <- self$getParameterValue("data")$samples
      return(sum(((data - self$mean()) / self$stdev())^3) / length(data))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      data <- self$getParameterValue("data")$samples
      kurt <- sum(((data - self$mean()) / self$stdev())^4) / length(data)
      if (excess) {
        return(kurt - 3)
      } else {
        return(kurt)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    #' @param ... Unused.
    entropy = function(base = 2, ...) {
      data <- self$getParameterValue("data")
      p <- data$N / nrow(data)
      return(-sum(p * log(p, base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      data <- self$getParameterValue("data")
      if (length(t) == 1) {
        return(sum(exp(data$samples * t) * (data$N / nrow(data))))
      } else {
        nr <- length(t)
        nc <- length(data$samples)
        return(as.numeric(
          exp(matrix(data$samples, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$N / nrow(data), nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      data <- self$getParameterValue("data")
      if (length(t) == 1) {
        return(sum(exp(data$samples * t * 1i) * (data$N / nrow(data))))
      } else {
        nr <- length(t)
        nc <- length(data$samples)
        return(as.complex(
          exp(matrix(data$samples * 1i, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$N / nrow(data), nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      data <- self$getParameterValue("data")
      if (length(z) == 1) {
        return(sum((z^data$samples) * (data$N / nrow(data))))
      } else {
        nr <- length(z)
        nc <- length(data$samples)
        return(as.numeric(
          (matrix(z, nrow = nr, ncol = nc)^matrix(data$samples, nrow = nr, ncol = nc,
                                                  byrow = z)) %*%
            matrix(data$N / nrow(data), nrow = nc, ncol = 1)
        ))
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      warning("Data cannot be updated after construction.")
      return(NULL)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      data <- self$getParameterValue("data")
      pdf <- as.numeric(unlist(data[match(
        round(x, 10),
        round(data$samples, 10)
      ), "N"] /
        nrow(data)))
      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("data")
      find <- findInterval(x, data$samples)
      find[find == 0] <- 1
      cdf <- as.numeric(unlist(data[find, "cumN"] / nrow(data)))
      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("data")
      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p

      p <- p * nrow(data)
      mat <- p <= matrix(data$cumN, nrow = length(p), ncol = nrow(data), byrow = T)
      which <- apply(mat, 1, function(x) which(x)[1])
      which[is.na(which)] <- ncol(mat)

      return(as.numeric(unlist(data[which, "samples"])))
    },
    .rand = function(n) {
      sample(self$getParameterValue("data")$samples, n, TRUE)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .data = data.table::data.table(),
    .total = numeric(1)
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Emp", ClassName = "Empirical",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = ""
  )
)
