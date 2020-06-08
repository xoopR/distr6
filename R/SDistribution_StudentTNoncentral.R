
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
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_df
#' @template param_location
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
StudentTNoncentral <- R6Class("StudentTNoncentral",
  inherit = SDistribution, lock_objects = F,
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
        symmetry = "sym",
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

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
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
