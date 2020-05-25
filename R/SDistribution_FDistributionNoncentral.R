
#' @name FDistributionNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName FDistributionNoncentral
#' @templateVar DistName Noncentral F
#' @templateVar uses in ANOVA testing and is the ratio of scaled Chi-Squared distributions
#' @templateVar params two degrees of freedom parameters, \eqn{\mu, \nu}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \sum_{r=0}^{\infty} ((exp(-\lambda/2)(\lambda/2)^r)/(B(\nu/2, \mu/2+r)r!))(\mu/\nu)^{\mu/2+r}(\nu/(\nu+x\mu))^{(\mu+\nu)/2+r}x^{\mu/2-1+r}}
#' @templateVar paramsupport \eqn{\mu, \nu > 0, \lambda \ge 0}
#' @templateVar distsupport the Positive Reals
#'
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_location
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
FDistributionNoncentral <- R6Class("FDistributionNoncentral", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "FDistributionNoncentral",
    short_name = "FNC",
    description = "Non-central F Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param df1 `(numeric(1))`\cr
    #' First degree of freedom of the distribution defined on the positive Reals.
    #' @param df2 `(numeric(1))`\cr
    #' Second degree of freedom of the distribution defined on the positive Reals.
    initialize = function(df1 = 1, df2 = 1, location = 0, decorators = NULL) {

      private$.parameters <- getParameterSet(self, df1, df2, location)
      self$setParameterValue(df1 = df1, df2 = df2, location = location)

      if (df1 == 1) {
        support <- PosReals$new(zero = FALSE)
      } else {
        support <- PosReals$new(zero = TRUE)
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
      if (self$getParameterValue("df2") > 2) {
        df1 <- self$getParameterValue("df1")
        df2 <- self$getParameterValue("df2")
        loc <- self$getParameterValue("location")
        return(df2 * (df1 + loc) / (df1 * (df2 - 2)))
      }
      else {
        return(NaN)
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      if (self$getParameterValue("df2") > 4) {
        df1 <- self$getParameterValue("df1")
        df2 <- self$getParameterValue("df2")
        loc <- self$getParameterValue("location")
        return(2 * (df2 / df1)^2 * ((df1 + loc)^2 + (df1 + 2 * loc) * (df2 - 2)) / ((df2 - 2)^2 * (df2 - 4)))
      }
      else {
        return(NaN)
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("df1") == 1) {
        private$.properties$support <- PosReals$new(zero = FALSE)
      } else {
        private$.properties$support <- PosReals$new(zero = TRUE)
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "df",
        x = x,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2),
          ncp = unlist(ncp)
        ),
        log = log,
        vec = test_list(df1)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "pf",
        x = x,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df1)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "qf",
        x = p,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(df1)
      )
    },
    .rand = function(n) {
      df1 <- self$getParameterValue("df1")
      df2 <- self$getParameterValue("df2")
      ncp <- self$getParameterValue("location")
      call_C_base_pdqr(
        fun = "rf",
        x = n,
        args = list(
          df1 = unlist(df1),
          df2 = unlist(df2),
          ncp = unlist(ncp)
        ),
        vec = test_list(df1)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$df1)) lst <- c(lst, list(df1 = paramlst$df1))
      if (!is.null(paramlst$df2)) lst <- c(lst, list(df2 = paramlst$df2))
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
    ShortName = "FNC", ClassName = "FDistributionNoncentral",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
