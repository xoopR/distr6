#' @name EmpiricalMV
#' @template SDist
#' @templateVar ClassName EmpiricalMV
#' @templateVar DistName EmpiricalMV
#' @templateVar uses in sampling such as MCMC
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{p(x) = \sum I(x = x_i) / k}
#' @templateVar paramsupport \eqn{x_i \epsilon R, i = 1,...,k}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @details
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the support set and uniform probabilities. The cdf assumes that the elements are supplied
#' in an indexed order (otherwise the results are meaningless).
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
#' @family multivariate distributions
#'
#' @export
EmpiricalMV <- R6Class("EmpiricalMV", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "EmpiricalMV",
    short_name = "EmpMV",
    description = "Multivariate Empirical Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param data `[matrix]` \cr
    #' Matrix-like object where each column is a vector of observed samples corresponding
    #' to each variable.
    #' @examples
    #' EmpiricalMV$new(MultivariateNormal$new()$rand(100))
    initialize = function(data, decorators = NULL) {

      data <- data.table::as.data.table(data)
      private$.variates = ncol(data)

      private$.parameters <- ParameterSet$new(
        id = "data",
        value = list(data),
        support = UniversalSet$new(),
        settable = FALSE,
        updateFunc = NULL,
        description = "Data"
      )

      super$initialize(
        decorators = decorators,
        support = do.call(setproduct, as.Tuple(data)),
        type = Reals$new()^ncol(data)
      )
    }, # TODO

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(mean(unlist(self$support$elements)))
    }, #TODO

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
    }, #TODO

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(sum((unlist(self$support$elements) - self$mean())^2) / private$.total)
    }, #TODO

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      return(sum(((unlist(self$support$elements) - self$mean()) / self$stdev())^3) / private$.total)
    }, #TODO

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
    }, #TODO

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      p <- private$.data$N / private$.total
      return(-sum(p * log(p, base)))
    }, #TODO

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
    }, #TODO

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
    }, #TODO

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
    }, #TODO

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      message("There are no parameters to set.")
      return(NULL)
    } # TODO
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      pdf <- C_EmpiricalMVPdf(x = x,
                              data = as.matrix(self$getParameterValue("data")))

      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- C_EmpiricalMVCdf(x = x,
                              data = as.matrix(self$getParameterValue("data")))

      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .rand = function(n) {
      data <- self$getParameterValue("data")
      rand <- matrix(ncol = ncol(data), nrow = n)
      for (i in seq_along(data)) {
        rand[, i] == sample(data[, i], n, TRUE)
      }
      return(rand)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "multivariate"),

    .data = data.table::data.table(),
    .total = numeric(1)
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "EmpMV", ClassName = "EmpiricalMV",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "multivariate",
    Package = "-"
  )
)
