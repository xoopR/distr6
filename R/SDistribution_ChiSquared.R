#' @name ChiSquared
#' @template SDist
#' @templateVar ClassName ChiSquared
#' @templateVar DistName Chi-Squared
#' @templateVar uses to model the sum of independent squared Normal distributions and for confidence intervals
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\nu/2-1} exp(-x/2))/(2^{\nu/2}\Gamma(\nu/2))}
#' @templateVar paramsupport \eqn{\nu > 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar constructor df = 1
#' @templateVar arg1 \code{df} \tab numeric \tab  degrees of freedom. \cr
#' @templateVar constructorDets \code{df} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{ChiSquaredNoncentral}} for the noncentral Chi-Squared distribution.
#'
#' @examples
#' x <- ChiSquared$new(df = 2)
#'
#' # Update parameters
#' x$setParameterValue(location = 3)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
NULL

ChiSquared <- R6Class("ChiSquared", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "ChiSquared",
    short_name = "ChiSq",
    description = "ChiSquared Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df)
      self$setParameterValue(df = df)

      if (df == 1) {
        support <- PosReals$new(zero = F)
      } else {
        support <- PosReals$new(zero = T)
      }

      super$initialize(
        decorators = decorators,
        support = support,
        type = PosReals$new(zero = TRUE)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(self$getParameterValue("df"))
    },
    mode = function() {
      return(max(self$getParameterValue("df") - 2, 0))
    },
    variance = function() {
      return(self$getParameterValue("df") * 2)
    },
    skewness = function() {
      return(sqrt(8 / self$getParameterValue("df")))
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(12 / self$getParameterValue("df"))
      } else {
        return(12 / self$getParameterValue("df") + 3)
      }
    },
    entropy = function(base = 2) {
      return(self$getParameterValue("df") / 2 + log(2 * gamma(self$getParameterValue("df") / 2), base) +
               ((1 - self$getParameterValue("df") / 2) * digamma(self$getParameterValue("df") / 2)))
    },
    mgf = function(t) {
      if (t < 0.5) {
        return((1 - 2 * t)^(-self$getParameterValue("df") / 2))
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      return((1 - 2i * t)^(-self$getParameterValue("df") / 2))
    },
    pgf = function(z) {
      if (z > 0 & z < sqrt(exp(1))) {
        return((1 - 2 * log(z))^(-self$getParameterValue("df") / 2))
      } else {
        return(NaN)
      }
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("df") == 1) {
        private$.properties$support <- PosReals$new(zero = F)
      } else {
        private$.properties$support <- PosReals$new(zero = T)
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "dchisq",
        x = x,
        args = list(df = unlist(df)),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "pchisq",
        x = x,
        args = list(df = unlist(df)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "qchisq",
        x = p,
        args = list(df = unlist(df)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .rand = function(n) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "rchisq",
        x = n,
        args = list(df = unlist(df)),
        vec = test_list(df)
      )
    },

      # getRefParams
      .getRefParams = function(paramlst) {
        lst <- list()
        if (!is.null(paramlst$df)) lst <- c(lst, list(df = paramlst$df))
        return(lst)
      },

      # traits
      .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "ChiSq", ClassName = "ChiSquared",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
