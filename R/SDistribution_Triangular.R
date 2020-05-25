
#' @name Triangular
#' @template SDist
#' @aliases SymmetricTriangular
#' @templateVar ClassName Triangular
#' @templateVar DistName Triangular
#' @templateVar uses to model population data where only the minimum, mode and maximum are known (or can be reliably estimated), also to model the sum of standard uniform distributions
#' @templateVar params lower limit, \eqn{a}, upper limit, \eqn{b}, and mode, \eqn{c},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \cr\cr \eqn{f(x) = 0, x < a} \cr \eqn{f(x) = 2(x-a)/((b-a)(c-a)), a \le x < c} \cr \eqn{f(x) = 2/(b-a), x = c} \cr \eqn{f(x) = 2(b-x)/((b-a)(b-c)), c < x \le b} \cr \eqn{f(x) = 0, x > b}
#' @templateVar paramsupport \eqn{a,b,c \ \in \ R}{a,b,c \epsilon R}, \eqn{a \le c \le b}
#' @templateVar distsupport \eqn{[a, b]}
#' @templateVar constructor lower = 0, upper = 1, mode = 0.5, symmetric = FALSE
#' @templateVar arg1 \code{lower} \tab numeric \tab lower limit. \cr
#' @templateVar arg2 \code{upper} \tab numeric \tab upper limit. \cr
#' @templateVar arg3 \code{mode} \tab numeric \tab mode. \cr
#' @templateVar arg4 \code{symmetric} \tab logical \tab see details. \cr
#' @templateVar constructorDets \code{lower}, \code{upper} and \code{mode} as numerics. If \code{symmetric = TRUE} then the \code{mode} parameter is determined automatically and is defined by \deqn{mode = (lower + upper) /2} this cannot be changed after construction. If \code{symmetric = FALSE} (default) then \code{mode} can be updated after construction.
#' @templateVar additionalSeeAlso \code{\link{Uniform}} for the Uniform distribution.
#'
#' @examples
#' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)
#' Triangular$new(lower = 2, upper = 5, symmetric = FALSE) # Note mode defaults to a symmetric shape
#' Triangular$new(lower = 2, upper = 5, mode = 4)
#'
#' # You can view the type of Triangular distribution with $description
#' Triangular$new(lower = 2, upper = 5, symmetric = TRUE)$description
#' Triangular$new(lower = 2, upper = 5, symmetric = FALSE)$description
#'
#' x <- Triangular$new(lower = -1, upper = 1)
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

Triangular <- R6Class("Triangular", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Triangular",
    short_name = "Tri",
    description = "Triangular Probability Distribution.",
    packages = "extraDistr",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(lower = 0, upper = 1, mode = (lower + upper) / 2,
                          symmetric = FALSE,
                          decorators = NULL) {


      if (symmetric) {
        self$description <- "Symmetric Triangular Probability Distribution."
        symmetry <- "symmetric"
      } else {
        self$description <- "Triangular Probability Distribution."
        private$.type <- "asymmetric"

        if (mode == (lower + upper) / 2) {
          symmetry <- "symmetric"
        } else {
          symmetry <- "asymmetric"
        }
      }

      private$.parameters <- getParameterSet(self, lower, upper, mode, symmetric)
      self$setParameterValue(lower = lower, upper = upper, mode = mode)

      super$initialize(
        decorators = decorators,
        support = Interval$new(lower, upper),
        symmetry = symmetry,
        type = Reals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return((self$getParameterValue("lower") + self$getParameterValue("upper") + self$getParameterValue("mode")) / 3)
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = NULL) {
      return(self$getParameterValue("mode"))
    },
    median = function() {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")
      if (mode >= (lower + upper)/2) {
        return(lower + sqrt((upper - lower)*(mode - lower))/sqrt(2))
      } else {
        return(upper - sqrt((upper - lower)*(upper - mode))/sqrt(2))
      }
    },
    variance = function() {
      return((self$getParameterValue("lower")^2 + self$getParameterValue("upper")^2 +
                self$getParameterValue("mode")^2 - self$getParameterValue("lower") * self$getParameterValue("upper") -
                self$getParameterValue("lower") * self$getParameterValue("mode") -
                self$getParameterValue("upper") * self$getParameterValue("mode")) / 18)
    },
    skewness = function() {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")

      num <- sqrt(2) * (lower + upper - 2 * mode) * (2 * lower - upper - mode) * (lower - 2 * upper + mode)
      den <- 5 * (lower^2 + upper^2 + mode^2 - lower * upper - lower * mode - upper * mode)^1.5
      return(num / den)
    },
    kurtosis = function(excess = TRUE) {
      if (excess) {
        return(-0.6)
      } else {
        return(2.4)
      }
    },
    entropy = function(base = 2) {
      return(0.5 * log((self$getParameterValue("upper") - self$getParameterValue("lower")) / 2, base))
    },
    mgf = function(t) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")

      num <- 2 * ((upper - mode) * exp(lower * t) - (upper - lower) * exp(mode * t) + (mode - lower) * exp(upper * t))
      den <- (upper - lower) * (mode - lower) * (upper - mode) * t^2

      return(num / den)
    },
    cf = function(t) {
      lower <- self$getParameterValue("lower")
      upper <- self$getParameterValue("upper")
      mode <- self$getParameterValue("mode")

      num <- -2 * ((upper - mode) * exp(1i * lower * t) - (upper - lower) * exp(1i * mode * t) + (mode - lower) * exp(1i * upper * t))
      den <- (upper - lower) * (mode - lower) * (upper - mode) * t^2

      return(num / den)
    },
    pgf = function(z) {
      return(NaN)
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if ("lower" %in% names(lst) & "upper" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] < lst[["upper"]], .var.name = "lower must be < upper")
      } else if ("lower" %in% names(lst)) {
        checkmate::assert(lst[["lower"]] < self$getParameterValue("upper"), .var.name = "lower must be < upper")
      } else if ("upper" %in% names(lst)) {
        checkmate::assert(lst[["upper"]] > self$getParameterValue("lower"), .var.name = "upper must be > lower")
      }

      if (private$.type != "symmetric") {
        if ("mode" %in% names(lst)) {
          if ("lower" %in% names(lst)) {
            checkmate::assert(lst[["mode"]] >= lst[["lower"]], .var.name = "mode must be >= lower")
          } else {
            checkmate::assert(lst[["mode"]] >= self$getParameterValue("lower"), .var.name = "mode must be >= lower")
          }
          if ("upper" %in% names(lst)) {
            checkmate::assert(lst[["mode"]] <= lst[["upper"]], .var.name = "mode must be <= upper")
          } else {
            checkmate::assert(lst[["mode"]] <= self$getParameterValue("upper"), .var.name = "mode must be <= upper")
          }
        }
      }

      super$setParameterValue(lst = lst, error = error)

      private$.properties$support <- Interval$new(self$getParameterValue("lower"), self$getParameterValue("upper"))
      if (private$.type != "symmetric") {
        if (self$getParameterValue("mode") == (self$getParameterValue("lower") + self$getParameterValue("upper")) / 2) {
          private$.properties$symmetry <- "symmetric"
        } else {
          private$.properties$symmetry <- "asymmetric"
        }
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::dtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(x = x, log = log)
        )
      } else {
        extraDistr::dtriang(
          x,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          log = log
        )
      }
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::ptriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(
            q = x,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::ptriang(
          x,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::qtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(
            p = p,
            lower.tail = lower.tail,
            log.p = log.p
          )
        )
      } else {
        extraDistr::qtriang(
          p,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    },
    .rand = function(n) {
      if (checkmate::testList(self$getParameterValue("lower"))) {
        mapply(
          extraDistr::rtriang,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode"),
          MoreArgs = list(n = n)
        )
      } else {
        extraDistr::rtriang(
          n,
          a = self$getParameterValue("lower"),
          b = self$getParameterValue("upper"),
          c = self$getParameterValue("mode")
        )
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$lower)) lst <- c(lst, list(lower = paramlst$lower))
      if (!is.null(paramlst$upper)) lst <- c(lst, list(upper = paramlst$upper))
      if (private$.type != "symmetric") {
        if (!is.null(paramlst$mode)) lst <- c(lst, list(mode = paramlst$mode))
      }
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "continuous", variateForm = "univariate"),

    .type = "symmetric"
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Tri", ClassName = "Triangular",
    Type = "\u211D", ValueSupport = "continuous",
    VariateForm = "univariate",
    Package = "extraDistr"
  )
)
