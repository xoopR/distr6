# nolint start
#' @name BetaNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName BetaNoncentral
#' @templateVar DistName Noncentral Beta
#' @templateVar uses as the prior in Bayesian modelling
#' @templateVar params two shape parameters, \eqn{\alpha, \beta}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = exp(-\lambda/2) \sum_{r=0}^\infty ((\lambda/2)^r/r!) (x^{\alpha+r-1}(1-x)^{\beta-1})/B(\alpha+r, \beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0, \lambda \ge 0}, where \eqn{B} is the Beta function
#' @templateVar distsupport \eqn{[0, 1]}
# nolint end
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_poslocation
#' @template field_packages
#'
#' @family continuous distributions
#' @family univariate distributions
#'
#' @export
BetaNoncentral <- R6Class("BetaNoncentral",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "BetaNoncentral",
    short_name = "BetaNC",
    description = "BetaNoncentral Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param shape1 `(numeric(1))`\cr
    #' First shape parameter, `shape1 > 0`.
    #' @param shape2 `(numeric(1))`\cr
    #' Second shape parameter, `shape2 > 0`.
    initialize = function(shape1 = 1, shape2 = 1, location = 0, decorators = NULL) {

      private$.parameters <- getParameterSet.BetaNoncentral(self, shape1, shape2, location)
      self$setParameterValue(shape1 = shape1, shape2 = shape2, location = location)

      super$initialize(
        decorators = decorators,
        support = Interval$new(0, 1),
        symmetry = if (shape1 == shape2) "sym" else "asym",
        type = PosReals$new(zero = T)
      )
    },

    # stats

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("shape1") == self$getParameterValue("shape2")) {
        private$.properties$symmetry <- "symmetric"
      } else {
        private$.properties$symmetry <- "asymmetric"
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "dbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2),
          ncp = unlist(ncp)
        ),
        log = log,
        vec = test_list(shape1)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "pbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "qbeta",
        x = p,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2),
          ncp = unlist(ncp)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .rand = function(n) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      ncp <- self$getParameterValue("location")

      call_C_base_pdqr(
        fun = "rbeta",
        x = n,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2),
          ncp = unlist(ncp)
        ),
        vec = test_list(shape1)
      )
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "BetaNC", ClassName = "BetaNoncentral",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats", Tags = ""
  )
)
