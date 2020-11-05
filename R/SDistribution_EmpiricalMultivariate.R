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
#' as the support set and uniform probabilities. Sampling is performed with replacement, which is
#' consistent with other distributions but non-standard for Empirical distributions. Use
#' [simulateEmpiricalDistribution] to sample without replacement.
#'
#' The cdf assumes that the elements are supplied in an indexed order
#' (otherwise the results are meaningless).
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
EmpiricalMV <- R6Class("EmpiricalMV",
  inherit = SDistribution, lock_objects = F,
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
    initialize = function(data = data.frame(1, 1), decorators = NULL) {

      if (ncol(data) == 1) {
        stop("Number of columns in `data` is '1', use Empirical distribution instead.")
      }

      support <- do.call(setproduct, as.Tuple(data))
      data <- data.table::as.data.table(data)
      private$.variates <- ncol(data)

      private$.parameters <- ParameterSet$new(
        id = "data",
        value = list(data),
        support = Universal$new(),
        settable = FALSE,
        description = "Data"
      )

      super$initialize(
        decorators = decorators,
        support = support,
        type = Reals$new()^"n"
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    #' @param ... Unused.
    mean = function(...) {
      as.numeric(colMeans(self$getParameterValue("data")))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      data <- self$getParameterValue("data")
      n <- nrow(data)
      as.numeric(apply(data, 2, function(x) var(x) * ((n - 1) / n)))
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
      pdf <- C_EmpiricalMVPdf(
        x = x,
        data = as.matrix(self$getParameterValue("data"))
      )

      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- C_EmpiricalMVCdf(
        x = x,
        data = as.matrix(self$getParameterValue("data"))
      )

      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .rand = function(n) {
      data <- as.matrix(self$getParameterValue("data"))
      rand <- matrix(ncol = ncol(data), nrow = n)
      for (i in seq(ncol(data))) {
        rand[, i] <- sample(data[, i], n, TRUE)
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
    Package = "-", Tags = ""
  )
)
