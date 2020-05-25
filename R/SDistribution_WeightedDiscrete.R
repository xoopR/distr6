
#' @name WeightedDiscrete
#' @template SDist
#' @templateVar ClassName WeightedDiscrete
#' @templateVar DistName WeightedDiscrete
#' @templateVar uses in empirical estimators such as Kaplan-Meier
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @details
#' Sampling from this distribution is performed with the [sample] function with the elements given
#' as the x values and the pdf as the probabilities. The cdf and quantile assume that the
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
#' @examples
#' x <- WeightedDiscrete$new(data = data.frame(x = 1:3, pdf = c(1 / 5, 3 / 5, 1 / 5)))
#' WeightedDiscrete$new(data = data.frame(x = 1:3, cdf = c(1 / 5, 4 / 5, 1))) # equivalently
#'
#' # d/p/q/r
#' x$pdf(1:5)
#' x$cdf(1:5) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
WeightedDiscrete <- R6Class("WeightedDiscrete", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "WeightedDiscrete",
    short_name = "WeightDisc",
    description = "Weighted Discrete Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param data `([data.frame])`\cr
    #' Data to define the distribution, should be coercable to a [data.frame]. `data` must include
    #' `x` as the first column and either one or both of `pdf` and `cdf`. Where they are the respective
    #' pdf and cdf of the corresponding `x` value.
    initialize = function(data, decorators = NULL) {

      data <- data.table::as.data.table(data)
      checkmate::assert(all(colnames(data) %in% c("pdf", "cdf", "x")),
                        .var.name = "data column names should be one of 'pdf', 'cdf', 'x"
      )
      checkmate::assert("x" %in% colnames(data),
                        .var.name = "'x' must be included in data column names"
      )
      checkmate::assert(any(c("pdf", "cdf") %in% colnames(data)),
                        .var.name = "at least one of 'pdf' and 'cdf' must be included in data column names"
      )

      if ("pdf" %in% colnames(data) & !("cdf" %in% colnames(data))) {
        data$cdf <- cumsum(data$pdf)
      } else if ("cdf" %in% colnames(data) & !("pdf" %in% colnames(data))) {
        data$pdf <- c(data$cdf[1], diff(data$cdf))
      }

      checkmate::assertNumeric(data$pdf, lower = 0, upper = 1, .var.name = "pdf is not valid")
      checkmate::assertNumeric(data$cdf, lower = 0, upper = 1, .var.name = "cdf is not valid")

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
        support = Set$new(data, class = "numeric"),
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      data <- self$getParameterValue("data")
      return(sum(data$x * data$pdf))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      data <- self$getParameterValue("data")

      if (which == "all") {
        return(data$x[data$pdf == max(data$pdf)])
      } else {
        return(data$x[data$pdf == max(data$pdf)][which])
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      data <- self$getParameterValue("data")
      return(sum((data$x - self$mean())^2 * data$pdf))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      data <- self$getParameterValue("data")
      return(sum(((data$x - self$mean()) / self$stdev())^3 * data$pdf))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      data <- self$getParameterValue("data")
      kurt <- sum(((data$x - self$mean()) / self$stdev())^4 * data$pdf)
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
      pdf <- self$getParameterValue("data")$pdf
      return(-sum(pdf * log(pdf, base)))
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      data <- self$getParameterValue("data")

      if (length(t) == 1) {
        return(sum(exp(data$x * t) * (data$pdf)))
      } else {
        nr <- length(t)
        nc <- length(data$x)
        return(as.numeric(
          exp(matrix(data$x, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      data <- self$getParameterValue("data")

      if (length(t) == 1) {
        return(sum(exp(data$x * t * 1i) * (data$pdf)))
      } else {
        nr <- length(t)
        nc <- length(data$x)
        return(as.complex(
          exp(matrix(data$x * 1i, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      data <- self$getParameterValue("data")

      if (length(z) == 1) {
        return(sum((z^data$x) * data$pdf))
      } else {
        nr <- length(z)
        nc <- length(data$x)
        return(as.numeric(
          (matrix(z, nrow = nr, ncol = nc)^matrix(data$x, nrow = nr, ncol = nc, byrow = z)) %*%
            matrix(data$pdf, nrow = nc, ncol = 1)
        ))
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      message("WeightedDiscrete cannot be modified after construction.")
      return(NULL)
    }

    # getParameterValue = function(id, error = "warn") {
    #   if ("data" %in% id) {
    #     return(super$getParameterValue("data", error))
    #   }
    # }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      data <- self$getParameterValue("data")

      if (checkmate::testList(data)) {
        data <- unlist(data)
        pdf <- matrix(as.numeric(data[names(data) == "pdf"]), ncol = nrow(data))
        data <- matrix(as.numeric(data[names(data) == "x"]), ncol = ncol(pdf))
        return(C_Vec_WeightedDiscretePdf(x, data$x, data$pdf, log))
      } else {
        return(C_WeightedDiscretePdf(x, data$x, data$pdf, log))
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("data")

      if (checkmate::testList(data)) {
        cdf <- matrix(as.numeric(data[names(data) == "cdf"]), ncol = nrow(data))
        data <- matrix(as.numeric(data[names(data) == "x"]), ncol = ncol(cdf))
        return(C_Vec_WeightedDiscreteCdf(x, data, cdf, lower.tail, log.p))
      } else {
        return(C_WeightedDiscreteCdf(x, data$x, data$cdf, lower.tail, log.p))
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      data <- self$getParameterValue("data")

      if (checkmate::testList(data)) {
        cdf <- matrix(as.numeric(data[names(data) == "cdf"]), ncol = nrow(data))
        data <- matrix(as.numeric(data[names(data) == "x"]), ncol = ncol(cdf))
        return(C_Vec_WeightedDiscreteQuantile(x, data, cdf, lower.tail, log.p))
      } else {
        return(C_WeightedDiscreteQuantile(x, data$x, data$cdf, lower.tail, log.p))
      }
    },
    .rand = function(n) {
      data <- self$getParameterValue("data")

      if (checkmate::testList(data)) {
        rand <- matrix(nrow = n, ncol = nrow(pdf))
        for (i in seq_along(data)) {
          rand[, i] <- sample(data[[i]]$x, n, TRUE, data[[i]]$pdf)
        }
      } else {
        rand <- sample(data$x, n, TRUE, data$pdf)
      }
      return(rand)
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$data)) lst <- lst$data = paramlst$data
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .data = "Deprecated - use self$getParameterValue instead."
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "WeightDisc", ClassName = "WeightedDiscrete",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-"
  )
)
