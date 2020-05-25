
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
#' Sampling from this distribution is performed with the  [sample] function with the elements
#' given as the support set and uniform probabilities. The cdf and quantile assumes that the
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
#' @export
Empirical <- R6Class("Empirical", inherit = SDistribution, lock_objects = F,
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
    initialize = function(samples, decorators = NULL) {

      samples <- sort(as.numeric(samples))

      private$.data <- data.table::as.data.table(table(samples))
      private$.data$samples <- as.numeric(private$.data$samples)
      private$.data <- cbind(private$.data, cumN = cumsum(private$.data$N))
      private$.total <- length(samples)

      super$initialize(
        decorators = decorators,
        support = Tuple$new(universe = Reals$new(), elements = as.list(samples), class = "numeric"),
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(mean(unlist(self$support$elements)))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (which == "all") {
        return(modal(unlist(self$support$elements)))
      } else {
        return(modal(unlist(self$support$elements))[which])
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(sum((unlist(self$support$elements) - self$mean())^2) / private$.total)
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      return(sum(((unlist(self$support$elements) - self$mean()) / self$stdev())^3) / private$.total)
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      kurt <- sum(((unlist(self$support$elements) - self$mean()) / self$stdev())^4) / private$.total
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
    entropy = function(base = 2) {
      p <- private$.data$N / private$.total
      return(-sum(p * log(p, base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      if (length(t) == 1) {
        return(sum(exp(private$.data$samples * t) * (private$.data$N / private$.total)))
      } else {
        nr <- length(t)
        nc <- length(private$.data$samples)
        return(as.numeric(
          exp(matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      if (length(t) == 1) {
        return(sum(exp(private$.data$samples * t * 1i) * (private$.data$N / private$.total)))
      } else {
        nr <- length(t)
        nc <- length(private$.data$samples)
        return(as.complex(
          exp(matrix(private$.data$samples * 1i, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      if (length(z) == 1) {
        return(sum((z^private$.data$samples) * (private$.data$N / private$.total)))
      } else {
        nr <- length(z)
        nc <- length(private$.data$samples)
        return(as.numeric(
          (matrix(z, nrow = nr, ncol = nc)^matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = z)) %*%
            matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      message("There are no parameters to set.")
      return(NULL)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      pdf <- as.numeric(unlist(private$.data[match(round(x, 10),
                                                   round(private$.data$samples, 10)), "N"] /
                                 private$.total))
      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      find <- findInterval(x, private$.data$samples)
      find[find == 0] <- 1
      cdf <- as.numeric(unlist(private$.data[find, "cumN"] / private$.total))
      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p

      p <- p * private$.total
      mat <- p <= matrix(private$.data$cumN, nrow = length(p), ncol = nrow(private$.data), byrow = T)
      which <- apply(mat, 1, function(x) which(x)[1])
      which[is.na(which)] <- ncol(mat)

      return(as.numeric(unlist(private$.data[which, "samples"])))
    },
    .rand = function(n) {
      sample(unlist(self$properties$support$elements), n, TRUE)
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
    Package = "-"
  )
)
