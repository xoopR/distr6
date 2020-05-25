
#' @name DiscreteUniform
#' @template SDist
#' @templateVar ClassName DiscreteUniform
#' @templateVar DistName Discrete Uniform
#' @templateVar uses as a discrete variant of the more popular Uniform distribution, used to model events with an equal probability of occurring (e.g. role of a die)
#' @templateVar params lower, \eqn{a}, and upper, \eqn{b}, limits
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = 1/(b - a + 1)}
#' @templateVar paramsupport \eqn{a, b \ \in \ Z; \ b \ge a}{a, b \epsilon Z; b \ge a}
#' @templateVar distsupport \eqn{\{a, a + 1,..., b\}}{{a, a + 1,..., b}}
#' @templateVar constructor lower = 0, upper = 1
#' @templateVar arg1 \code{lower} \tab integer \tab lower distribution limit. \cr
#' @templateVar arg2 \code{upper} \tab integer \tab upper distribution limit. \cr
#' @templateVar constructorDets \code{lower} and \code{upper} as whole numbers.
#' @templateVar additionalSeeAlso \code{\link{Uniform}} for the (continuous) Uniform distribution.
#'
#' @examples
#' x <- DiscreteUniform$new(lower = -10, upper = 5)
#'
#' # Update parameters
#' x$setParameterValue(lower = 2, upper = 7)
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

DiscreteUniform <- R6Class("DiscreteUniform", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "DiscreteUniform",
    short_name = "DUnif",
    description = "Discrete Uniform Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(lower = 0, upper = 1, decorators = NULL) {

      private$.parameters <- getParameterSet(self, lower, upper)
      self$setParameterValue(lower = lower, upper = upper)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper, class = "integer"),
        symmetry = "sym",
        type = Integers$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return((self$getParameterValue("lower") + self$getParameterValue("upper")) / 2)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      if (which == "all") {
        return(self$inf:self$sup)
      } else {
        return((self$inf:self$sup)[which])
      }
    },
    variance = function() {
      return(((self$getParameterValue("upper") - self$getParameterValue("lower") + 1)^2 - 1) / 12)
    },
    skewness = function() {
      return(0)
    },
    kurtosis = function(excess = TRUE) {
      exkurtosis <- (-6 * (self$getParameterValue("N")^2 + 1)) / (5 * (self$getParameterValue("N")^2 - 1))
      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },
    entropy = function(base = 2) {
      return(log(self$getParameterValue("N"), base))
    },
    mgf = function(t) {
      num <- exp(t * self$getParameterValue("lower")) - exp((self$getParameterValue("upper") + 1) * t)
      denom <- self$getParameterValue("N") * (1 - exp(t))
      return(num / denom)
    },
    cf = function(t) {
      num <- exp(1i * t * self$getParameterValue("lower")) - exp((self$getParameterValue("upper") + 1) * t * 1i)
      denom <- self$getParameterValue("N") * (1 - exp(1i * t))
      return(num / denom)
    },
    pgf = function(z) {
      return(1 / self$getParameterValue("N") * sum(z^(1:self$getParameterValue("N"))))
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if ("lower" %in% names(lst) & "upper" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] <= lst[["upper"]], .var.name = "lower must be <= upper")
      } else if ("lower" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] <= self$getParameterValue("upper"), .var.name = "lower must be <= upper")
      } else if ("upper" %in% names(lst)) {
        checkmate::assert(lst[["upper"]] >= self$getParameterValue("lower"), .var.name = "upper must be >= lower")
      }

      super$setParameterValue(lst = lst, error = error)
      private$.properties$support <- Set$new(self$getParameterValue("lower"):self$getParameterValue("upper"))
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::ddunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::ddunif(
          x,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::pdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::pdunif(
          x,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::qdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qdunif(
          p,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::rdunif,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rdunif(
          n,
          min = self$getParameterValue("lower"),
          max = self$getParameterValue("upper")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$lower)) lst <- c(lst, list(lower = paramlst$lower))
      if (!is.null(paramlst$upper)) lst <- c(lst, list(upper = paramlst$upper))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "DUnif", ClassName = "DiscreteUniform",
    Type = "\u2124", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
