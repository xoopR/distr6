# nolint start
#' @name NegativeBinomial
#' @template SDist
#' @templateVar ClassName NegativeBinomial
#' @templateVar DistName Negative Binomial
#' @templateVar uses to model the number of successes, trials or failures before a given number of failures or successes
#' @templateVar params number of failures before successes, \eqn{n}, and probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(x + n - 1, n - 1) p^n (1 - p)^x}
#' @templateVar paramsupport \eqn{n = {0,1,2,\ldots}} and probability \eqn{p}, where \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{{0,1,2,\ldots}} (for fbs and sbf) or \eqn{{n,n+1,n+2,\ldots}} (for tbf and tbs) (see below)
# nolint end
#' @details
#' The Negative Binomial distribution can refer to one of four distributions (forms):
#'
#' 1. The number of failures before K successes (fbs)
#' 2. The number of successes before K failures (sbf)
#' 3. The number of trials before K failures (tbf)
#' 4. The number of trials before K successes (tbs)
#'
#' For each we refer to the number of K successes/failures as the \code{size} parameter.
#'
#' Note that the `size` parameter is not currently vectorised in [VectorDistribution]s.
#'
#' @template param_prob
#' @template param_qprob
#' @template class_distribution
#' @template method_mode
#' @template method_entropy
#' @template method_kurtosis
#' @template method_pgf
#' @template method_mgfcf
#' @template method_setParameterValue
#' @template param_decorators
#' @template field_packages
#'
#' @family discrete distributions
#' @family univariate distributions
#'
#' @export
NegativeBinomial <- R6Class("NegativeBinomial",
  inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "NegativeBinomial",
    short_name = "NBinom",
    description = "Negative Binomial Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param size `(integer(1))`\cr
    #' Number of trials/successes.
    #' @param mean `(numeric(1))`\cr
    #' Mean of distribution, alternative to `prob` and `qprob`.
    #' @param form `character(1))`\cr
    #' Form of the distribution, cannot be changed after construction. Options are to model
    #' the number of,
    #' * `"fbs"` - Failures before successes.
    #' * `"sbf"` - Successes before failures.
    #' * `"tbf"` - Trials before failures.
    #' * `"tbs"` - Trials before successes.
    #' Use `$description` to see the Negative Binomial form.
    initialize = function(size = 10, prob = 0.5, qprob = NULL, mean = NULL,
                          form = c("fbs", "sbf", "tbf", "tbs"),
                          decorators = NULL) {

      form <- match.arg(form)

      private$.parameters <- getParameterSet(self, size, prob, qprob, mean, form)
      self$setParameterValue(size = size, prob = prob, qprob = qprob, mean = mean)

      if (form == "fbs") {
        support <- Naturals$new()
        self$description <- "Negative Binomial (fbs) Probability Distribution."
      } else if (form == "sbf") {
        support <- Naturals$new()
        self$description <- "Negative Binomial (sbf) Probability Distribution."
      } else if (form == "tbf") {
        support <- Interval$new(size, Inf, type = "[)", class = "integer")
        self$description <- "Negative Binomial (tbf) Probability Distribution."
      } else {
        support <- Interval$new(size, Inf, type = "[)", class = "integer")
        self$description <- "Negative Binomial (tbs) Probability Distribution."
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
    #' @param ... Unused.
    mean = function(...) {
      unlist(self$getParameterValue("mean"))
    },

    #' @description
    #' The mode of a probability distribution is the point at which the pdf is
    #' a local maximum, a distribution can be unimodal (one maximum) or multimodal (several
    #' maxima).
    mode = function(which = "all") {
      form <- self$getParameterValue("form")[[1]]
      size <- unlist(self$getParameterValue("size"))
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      if (form %in% c("sbf", "tbf")) {
        p <- prob
        q <- qprob
      } else {
        p <- qprob
        q <- prob
      }
      mode <- numeric(length(size))
      mode[size > 1] <- floor(((size - 1) * p) / q)

      if (form %in% c("tbf", "tbs")) {
        mode[size > 1] <- mode[size > 1] + size[size > 1]
      }

      return(mode)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      form <- self$getParameterValue("form")[[1]]
      size <- unlist(self$getParameterValue("size"))
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      if (form %in% c("sbf", "tbf")) {
        return(size * prob / (qprob^2))
      } else {
        return(size * qprob / (prob^2))
      }
    },

    #' @description
    #' The skewness of a distribution is defined by the third standardised moment,
    #' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' @param ... Unused.
    skewness = function(...) {
      form <- self$getParameterValue("form")[[1]]
      size <- unlist(self$getParameterValue("size"))
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      if (form %in% c("sbf", "tbf")) {
        return((1 + prob) / sqrt(size * prob))
      } else {
        return((1 + qprob) / sqrt(size * qprob))
      }
    },

    #' @description
    #' The kurtosis of a distribution is defined by the fourth standardised moment,
    #' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
    #' where \eqn{E_X} is the expectation of distribution X, \eqn{\mu} is the mean of the
    #' distribution and \eqn{\sigma} is the standard deviation of the distribution.
    #' Excess Kurtosis is Kurtosis - 3.
    #' @param ... Unused.
    kurtosis = function(excess = TRUE, ...) {
      form <- self$getParameterValue("form")[[1]]
      size <- unlist(self$getParameterValue("size"))
      prob <- unlist(self$getParameterValue("prob"))
      qprob <- 1 - prob

      if (form %in% c("sbf", "tbf")) {
        exkurtosis <- (qprob^2 - 6 * qprob + 6) / (size * prob)
      } else {
        exkurtosis <- (prob^2 - 6 * prob + 6) / (size * qprob)
      }

      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },

    #' @description The moment generating function is defined by
    #' \deqn{mgf_X(t) = E_X[exp(xt)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    mgf = function(t, ...) {
      form <- self$getParameterValue("form")
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob

      if (t < -log(prob)) {
        if (form %in% c("sbf", "tbf")) {
          return((qprob / (1 - prob * exp(t)))^size)
        } else {
          return((prob / (1 - qprob * exp(t)))^size)
        }
      } else {
        return(NaN)
      }
    },

    #' @description The characteristic function is defined by
    #' \deqn{cf_X(t) = E_X[exp(xti)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    cf = function(t, ...) {
      form <- self$getParameterValue("form")
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob

      if (form %in% c("sbf", "tbf")) {
        return((qprob / (1 - prob * exp(t * 1i)))^size)
      } else {
        return((prob / (1 - qprob * exp(t * 1i)))^size)
      }
    },

    #' @description The probability generating function is defined by
    #' \deqn{pgf_X(z) = E_X[exp(z^x)]}
    #' where X is the distribution and \eqn{E_X} is the expectation of the distribution X.
    #' @param ... Unused.
    pgf = function(z, ...) {
      form <- self$getParameterValue("form")
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")
      qprob <- 1 - prob

      if (abs(z) < 1 / prob) {
        if (form == "sbf") {
          return((qprob / (1 - prob * z))^size)
        } else if (form == "tbs") {
          return(((prob * z) / (1 - qprob * z))^size)
        } else if (form == "fbs") {
          return((prob / (1 - qprob * z))^size)
        } else if (form == "tbf") {
          return(((qprob * z) / (1 - prob * z))^size)
        }
      } else {
        return(NaN)
      }
    },

    # optional setParameterValue
    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) lst <- list(...)
      if (!is.null(lst$mean)) {
        lst$prob <- NULL
        lst$qprob <- NULL
      } else if (!is.null(lst$qprob)) {
        lst$prob <- NULL
      }
      super$setParameterValue(lst = lst, error = error)

      form <- self$getParameterValue("form")[[1]]
      if (form == "tbf" | form == "tbs") {
        private$.properties$support <- Interval$new(self$getParameterValue("size"),
                                                    Inf, type = "[)", class = "integer")
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {

      size <- unlist(self$getParameterValue("size"))[[1]]
      prob <- unlist(self$getParameterValue("prob"))
      form <- self$getParameterValue("form")[[1]]

      if (form %in% c("sbf", "tbf")) {
        prob <- 1 - prob
      }
      if (form %in% c("tbs", "tbf")) {
        x <- x - size
      }

      return(
        call_C_base_pdqr(
          fun = "dnbinom",
          x = x,
          args = list(
            size = size,
            prob = prob
          ),
          log = log,
          vec = test_list(self$getParameterValue("size"))
        )
      )
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {

      size <- unlist(self$getParameterValue("size"))[[1]]
      prob <- unlist(self$getParameterValue("prob"))
      form <- self$getParameterValue("form")[[1]]

      if (form %in% c("sbf", "tbf")) {
        prob <- 1 - prob
      }
      if (form %in% c("tbs", "tbf")) {
        x <- x - size
      }

      return(
        call_C_base_pdqr(
        fun = "pnbinom",
        x = x,
        args = list(
          size = size,
          prob = prob
        ),
        log = log.p,
        lower.tail = lower.tail,
        vec = test_list(self$getParameterValue("size"))
        )
      )
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {

      size <- unlist(self$getParameterValue("size"))[[1]]
      prob <- unlist(self$getParameterValue("prob"))
      form <- self$getParameterValue("form")[[1]]

      if (form %in% c("sbf", "tbf")) {
        prob <- 1 - prob
      }

      quantile <- call_C_base_pdqr(
          fun = "qnbinom",
          x = p,
          args = list(
            size = size,
            prob = prob
          ),
          log = log.p,
          lower.tail = lower.tail,
          vec = test_list(self$getParameterValue("size"))
        )

      if (form %in% c("tbs", "tbf")) {
        quantile <- quantile + size
      }

      return(quantile)
    },
    .rand = function(n) {
      size <- unlist(self$getParameterValue("size"))[[1]]
      prob <- unlist(self$getParameterValue("prob"))
      form <- self$getParameterValue("form")[[1]]

      if (form %in% c("sbf", "tbf")) {
        prob <- 1 - prob
      }

      rand <- call_C_base_pdqr(
        fun = "rnbinom",
        x = n,
        args = list(
          size = size,
          prob = prob
        ),
        log = log.p,
        lower.tail = lower.tail,
        vec = test_list(self$getParameterValue("size"))
      )

      if (form %in% c("tbs", "tbf")) {
        rand <- rand + size
      }

      return(rand)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "univariate")
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "NBinom", ClassName = "NegativeBinomial",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-", Tags = "limits"
  )
)
