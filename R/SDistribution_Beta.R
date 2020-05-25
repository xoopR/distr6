
#' @name Beta
#' @template SDist
#' @templateVar ClassName Beta
#' @templateVar DistName Beta
#' @templateVar uses as the prior in Bayesian modelling
#' @templateVar params two shape parameters, \eqn{\alpha, \beta},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = (x^{\alpha-1}(1-x)^{\beta-1}) / B(\alpha, \beta)}
#' @templateVar paramsupport \eqn{\alpha, \beta > 0}, where \eqn{B} is the Beta function
#' @templateVar distsupport \eqn{[0, 1]}
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar constructor shape1 = 1, shape2 = 1
#' @templateVar arg1 \code{shape1, shape2} \tab numeric \tab positive shape parameter. \cr
#' @templateVar constructorDets  \code{shape1} and \code{shape2} as positive numerics.
#'
#' @examples
#' x <- Beta$new(shape1 = 2, shape2 = 5)
#'
#' # Update parameters
#' x$setParameterValue(shape1 = 1)
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

Beta <- R6Class("Beta", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Beta",
    short_name = "Beta",
    description = "Beta Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(shape1 = 1, shape2 = 1, decorators = NULL) {

      private$.parameters <- getParameterSet.Beta(self, shape1, shape2)
      self$setParameterValue(shape1 = shape1, shape2 = shape2)

      super$initialize(
        decorators = decorators,
        support = Interval$new(0, 1),
        symmetric = if (shape1 == shape2) "sym" else "asym",
        type = PosReals$new(zero = T)
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(self$getParameterValue("shape1") / (self$getParameterValue("shape1") + self$getParameterValue("shape2")))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (self$getParameterValue("shape1") <= 1 & self$getParameterValue("shape2") > 1) {
        return(0)
      } else if (self$getParameterValue("shape1") > 1 & self$getParameterValue("shape2") <= 1) {
        return(1)
      } else if (self$getParameterValue("shape1") < 1 & self$getParameterValue("shape2") < 1) {
        if (which == "all") {
          return(c(0, 1))
        } else {
          return(c(0, 1)[which])
        }
      } else if (self$getParameterValue("shape1") > 1 & self$getParameterValue("shape2") > 1) {
        return((self$getParameterValue("shape1") - 1) / (self$getParameterValue("shape1") + self$getParameterValue("shape2") - 2))
      }
    },
    variance = function() {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      return(shape1 * shape2 * ((shape1 + shape2)^-2) * (shape1 + shape2 + 1)^-1)
    },
    skewness = function() {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      return(2 * (shape2 - shape1) * ((shape1 + shape2 + 1)^0.5) * ((shape1 + shape2 + 2)^-1) * ((shape1 * shape2)^-0.5))
    },
    kurtosis = function(excess = TRUE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")

      ex_kurtosis <- 6 *
        {
          ((shape1 - shape2)^2) * (shape1 + shape2 + 1) - (shape1 * shape2 * (shape1 + shape2 + 2))
        } /
        (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))
      if (excess) {
        return(ex_kurtosis)
      } else {
        return(ex_kurtosis + 3)
      }
    },
    entropy = function(base = 2) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      return(log(beta(shape1, shape2), base) - ((shape1 - 1) * digamma(shape1)) -
               ((shape2 - 1) * digamma(shape2)) + ((shape1 + shape2 - 2) * digamma(shape1 + shape2)))
    },
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
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
      call_C_base_pdqr(
        fun = "dbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        log = log,
        vec = test_list(shape1)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "pbeta",
        x = x,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "qbeta",
        x = p,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(shape1)
      )
    },
    .rand = function(n) {
      shape1 <- self$getParameterValue("shape1")
      shape2 <- self$getParameterValue("shape2")
      call_C_base_pdqr(
        fun = "rbeta",
        x = n,
        args = list(
          shape1 = unlist(shape1),
          shape2 = unlist(shape2)
        ),
        vec = test_list(shape1)
      )
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$shape1)) lst <- c(lst, list(shape1 = paramlst$shape1))
      if (!is.null(paramlst$shape2)) lst <- c(lst, list(shape2 = paramlst$shape2))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Beta", ClassName = "Beta",
    Type = "\u211D+", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "stats"
  )
)
