
#' @name StudentT
#' @author Chijing Zeng
#' @template SDist
#' @templateVar ClassName StudentT
#' @templateVar DistName Student's T
#' @templateVar uses to estimate the mean of populations with unknown variance from a small sample size, as well as in t-testing for difference of means and regression analysis
#' @templateVar params degrees of freedom, \eqn{\nu},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \Gamma((\nu+1)/2)/(\sqrt(\nu\pi)\Gamma(\nu/2)) * (1+(x^2)/\nu)^(-(\nu+1)/2)}
#' @templateVar paramsupport \eqn{\nu > 0}
#' @templateVar distsupport the Reals
#' @templateVar constructor df = 1
#' @templateVar arg1 \code{df} \tab numeric \tab degrees of freedom. \cr
#' @templateVar constructorDets \code{df} as a positive numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{StudentTNoncentral}} for the noncentral Student's T distribution.
#'
#' @examples
#' x <- StudentT$new(df = 2)
#'
#' # Update parameters
#' x$setParameterValue(df = 3)
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

StudentT <- R6Class("StudentT", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "StudentT",
    short_name = "T",
    description = "Student's T Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, decorators = NULL, verbose = FALSE) {

      private$.parameters <- getParameterSet(self, df, verbose)
      self$setParameterValue(df = df)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetry = "sym",
        type = Reals$new()
      )
    },

    # stats
    mean = function() {
      df <- self$getParameterValue("df")
      if (df > 1) {
        return(0)
      } else {
        return(NaN)
      }
    },
    mode = function(which = NULL) {
      return(0)
    },
    variance = function() {
      df <- self$getParameterValue("df")
      if (df > 2) {
        return(df / (df - 2))
      } else {
        return(NaN)
      }
    },
    skewness = function() {
      if (self$getParameterValue("df") > 3) {
        return(0)
      } else {
        return(NaN)
      }
    },
    kurtosis = function(excess = TRUE) {
      df <- self$getParameterValue("df")
      if (df > 4) {
        exkurtosis <- 6 / (df - 4)
      } else {
        return(NaN)
      }

      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },
    entropy = function(base = 2) {
      df <- self$getParameterValue("df")
      return((((df + 1) / 2) * (digamma((1 + df) / 2) - digamma(df / 2))) + (log(sqrt(df) * beta(df / 2, 1 / 2), base)))
    },
    mgf = function(t) {
      return(NaN)
    },
    cf = function(t) {
      df <- self$getParameterValue("df")
      return((besselK(sqrt(df) * abs(t), df / 2) * ((sqrt(df) * abs(t))^(df / 2))) / (gamma(df / 2) * 2^(df / 2 - 1)))
    },
    pgf = function(z) {
      return(NaN)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "dt",
        x = x,
        args = list(df = unlist(df)),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      call_C_base_pdqr(
        fun = "pt",
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
        fun = "qt",
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
        fun = "rt",
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
    ShortName = "T", ClassName = "StudentT",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
