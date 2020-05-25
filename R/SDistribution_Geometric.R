
#' @name Geometric
#' @template SDist
#' @templateVar ClassName Geometric
#' @templateVar DistName Geometric
#' @templateVar uses to model the number of trials (or number of failures) before the first success
#' @templateVar params probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (1 - p)^{k-1}p}
#' @templateVar paramsupport \eqn{p \epsilon [0,1]}
#' @templateVar distsupport the Naturals (zero is included if modelling number of failures before success)
#' @templateVar additionalDetails The Geometric distribution is used to either refer to modelling the number of trials or number of failures before the first success.
#' @templateVar constructor prob = 0.5, qprob = NULL, trials = FALSE
#' @templateVar arg1 \code{prob} \tab numeric \tab probability of success. \cr
#' @templateVar arg2 \code{qprob} \tab numeric \tab probability of failure. \cr
#' @templateVar arg3 \code{trials} \tab logical \tab number of trials or failures, see details. \cr
#' @templateVar constructorDets \code{prob} or \code{qprob} as a number between 0 and 1. These are related via, \deqn{qprob = 1 - prob} If \code{qprob} is given then {prob is ignored}. \cr\cr The logical parameter \code{trials} determines which Geometric distribution is constructed and cannot be changed after construction. If \code{trials} is TRUE then the Geometric distribution that models the number of trials, \eqn{x}, before the first success is constructed. Otherwise the Geometric distribution calculates the probability of \eqn{y} failures before the first success. Mathematically these are related by \eqn{Y = X - 1}.
#'
#' @examples
#' # Different parameterisations
#' Geometric$new(prob = 0.2)
#' Geometric$new(qprob = 0.7)
#'
#' # Different forms of the distribution
#' Geometric$new(trials = TRUE) # Number of trials before first success
#' Geometric$new(trials = FALSE) # Number of failures before first success
#'
#' # Use description to see which form is used
#' Geometric$new(trials = TRUE)$description
#' Geometric$new(trials = FALSE)$description
#'
#' # Default is prob = 0.5 and number of failures before first success
#' x <- Geometric$new()
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(qprob = 0.2)
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

Geometric <- R6Class("Geometric", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "Geometric",
    short_name = "Geom",
    description = "Geometric Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(prob = 0.5, qprob = NULL, trials = FALSE, decorators = NULL,
                          verbose = FALSE) {

      private$.trials <- checkmate::assertLogical(trials)
      private$.parameters <- getParameterSet(x = self, prob = prob, qprob = qprob, trials = trials, verbose = verbose)
      self$setParameterValue(prob = prob, qprob = qprob)

      if (!trials) {
        support <- Naturals$new()
        self$description <- "Geometric (Failures) Probability Distribution."
      } else {
        support <- PosNaturals$new()
        self$description <- "Geometric (Trials) Probability Distribution."
      }

      super$initialize(
        decorators = decorators,
        support = support,
        type = Naturals$new()
      )
    },

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      if (private$.trials) {
        return(1 / self$getParameterValue("prob"))
      } else {
        return((1 - self$getParameterValue("prob")) / self$getParameterValue("prob"))
      }
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = 'all') {
      if (private$.trials) {
        return(1)
      } else {
        return(0)
      }
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return((1 - self$getParameterValue("prob")) / (self$getParameterValue("prob")^2))
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the distribution and
    #' \eqn{\sigma} is the standard deviation of the distribution.
    skewness = function() {
      return((2 - self$getParameterValue("prob")) / sqrt(1 - self$getParameterValue("prob")))
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    kurtosis = function(excess = TRUE) {
      exkurtosis <- 6 + (self$getParameterValue("prob")^2 / (1 - self$getParameterValue("prob")))
      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },

    #' @description
    #' The entropy of a (discrete) distribution is defined by
    #' \deqn{- \sum (f_X)log(f_X)}
    #' where \eqn{f_X} is the pdf of distribution X, with an integration analogue for
    #' continuous distributions.
    entropy = function(base = 2) {
      prob <- self$getParameterValue("prob")
      return(((-(1 - prob) * log(1 - prob, base)) - (prob * log(prob, base))) / prob)
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    mgf = function(t) {
      if (private$.trials) {
        if (t < -log(1 - self$getParameterValue("prob"))) {
          return((self$getParameterValue("prob") * exp(t)) / (1 - (1 - self$getParameterValue("prob")) * exp(t)))
        } else {
          return(NaN)
        }
      } else {
        return((self$getParameterValue("prob")) / (1 - (1 - self$getParameterValue("prob")) * exp(t)))
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    cf = function(t) {
      if (private$.trials) {
        return((self$getParameterValue("prob") * exp(1i * t)) / (1 - (1 - self$getParameterValue("prob")) * exp(1i * t)))
      } else {
        return((self$getParameterValue("prob")) / (1 - (1 - self$getParameterValue("prob")) * exp(1i * t)))
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    pgf = function(z) {
      if (private$.trials) {
        return((self$getParameterValue("prob") * z) / (1 - z * self$getParameterValue("qprob")))
      } else {
        return(self$getParameterValue("prob") / (1 - z * self$getParameterValue("qprob")))
      }
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      if (private$.trials) {
        x <- x + 1
      }

      prob <- self$getParameterValue("prob")
      call_C_base_pdqr(
        fun = "dgeom",
        x = x,
        args = list(prob = unlist(prob)),
        log = log,
        vec = test_list(prob)
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      if (private$.trials) {
        x <- x + 1
      }

      prob <- self$getParameterValue("prob")
      call_C_base_pdqr(
        fun = "pgeom",
        x = x,
        args = list(prob = unlist(prob)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      prob <- self$getParameterValue("prob")
      geom <- call_C_base_pdqr(
        fun = "qgeom",
        x = p,
        args = list(prob = unlist(prob)),
        lower.tail = lower.tail,
        log = log.p,
        vec = test_list(prob)
      )

      if (private$.trials) {
        geom <- geom + 1
      }

      return(geom)
    },
    .rand = function(n) {
      prob <- self$getParameterValue("prob")
      geom <- call_C_base_pdqr(
        fun = "rgeom",
        x = n,
        args = list(prob = unlist(prob)),
        vec = test_list(prob)
      )

      if (private$.trials) {
        geom <- geom + 1
      }

      return(geom)
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$prob)) lst <- c(lst, list(prob = paramlst$prob))
      if (!is.null(paramlst$qprob)) lst <- c(lst, list(prob = 1 - paramlst$qprob))
      return(lst)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate"),

    .trails = logical(0)
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "Geom", ClassName = "Geometric",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "stats"
  )
)
