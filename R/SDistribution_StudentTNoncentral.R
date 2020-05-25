
#' @name StudentTNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName StudentTNoncentral
#' @templateVar DistName Noncentral Student's T
#' @templateVar uses to estimate the mean of populations with unknown variance from a small sample size, as well as in t-testing for difference of means and regression analysis
#' @templateVar params degrees of freedom, \eqn{\nu} and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (\nu^{\nu/2}exp(-(\nu\lambda^2)/(2(x^2+\nu)))/(\sqrt{\pi} \Gamma(\nu/2) 2^{(\nu-1)/2} (x^2+\nu)^{(\nu+1)/2}))\int_{0}^{\infty} y^\nu exp(-1/2(y-x\lambda/\sqrt{x^2+\nu})^2)}
#' @templateVar paramsupport \eqn{\nu > 0}, \eqn{\lambda \epsilon R}
#' @templateVar distsupport the Reals
#' @templateVar omittedVars \code{skewness}, \code{kurtosis}, \code{mode}, \code{entropy}, \code{pgf}, \code{mgf} and \code{cf}
#' @templateVar constructor df = 1, location = 0
#' @templateVar arg1 \code{df} \tab numeric \tab degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab location (ncp in rstats). \cr
#' @templateVar constructorDets \code{df} as positive numeric and \code{location} as real numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}} for the Normal distribution, \code{\link{StudentT}} for the central Student's T distribution.
#'
#' @examples
#' x <- StudentTNoncentral$new(df = 2, location = 3)
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

StudentTNoncentral <- R6Class("StudentTNoncentral", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "StudentTNoncentral",
    short_name = "TNS",
    description = "Non-central Student's T Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(df = 1, location = 0, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df, location)
      self$setParameterValue(df = df, location = location)

      super$initialize(
        decorators = decorators,
        support = Reals$new(),
        symmetric = "sym",
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      df <- self$getParameterValue("df")
      if (df > 1) {
        return(self$getParameterValue("location") * sqrt(df / 2) * gamma((df - 1) / 2) / gamma(df / 2))
      } else {
        return(NaN)
      }
    },
    variance = function() {
      df <- self$getParameterValue("df")
      mu <- self$getParameterValue("location")
      if (df > 2) {
        return(df * (1 + mu^2) / (df - 2) - (mu^2 * df / 2) * (gamma((df - 1) / 2) / gamma(df / 2))^2)
      } else {
        return(NaN)
      }
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "dt",
        x = x,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        log = log,
        vec = test_list(df)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "pt",
        x = x,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "qt",
        x = p,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df)
      )
    },
    .rand = function(n) {
      df <- self$getParameterValue("df")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "rt",
        x = n,
        args = list(
          df = unlist(df),
          ncp = unlist(ncp)
        ),
        vec = test_list(df)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$df)) lst <- c(lst, list(df = paramlst$df))
      if (!is.null(paramlst$location)) lst <- c(lst, list(location = paramlst$location))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "TNC", ClassName = "StudentTNoncentral",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
