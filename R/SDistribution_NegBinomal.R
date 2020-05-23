
#' @name NegativeBinomial
#' @template SDist
#' @templateVar ClassName NegativeBinomial
#' @templateVar DistName Negative Binomial
#' @templateVar uses to model the number of successes, trials or failures before a given number of failures or successes
#' @templateVar params number of failures before successes, \eqn{n}, and probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(x + n - 1, n - 1) p^n (1 - p)^x}
#' @templateVar paramsupport \eqn{n = {0,1,2,\ldots}} and \eqn{p \epsilon [0,1]}, where \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{{0,1,2,\ldots}} (for fbs and sbf) or \eqn{{n,n+1,n+2,\ldots}} (for tbf and tbs) (see below)
#' @templateVar omittedVars `entropy`
#' @templateVar additionalDetails The Negative Binomial distribution can refer to one of four distributions (forms): \cr\cr 1. The number of failures before K successes (fbs) \cr\cr 2. The number of successes before K failures (sbf) \cr\cr 3. The number of trials before K failures (tbf) \cr\cr 4. The number of trials before K successes (tbs) \cr\cr For each we refer to the number of K successes/failures as the \code{size} parameter, \code{prob} is always the probability of success and \code{qprob} is the probability of failure. Use \code{$description} to see the Negative Binomial form.
#' @templateVar constructor size = 10, prob = 0.5, qprob = NULL, mean = NULL, form = "fbs"
#' @templateVar arg1 \code{size} \tab numeric \tab number of failures/successes. \cr
#' @templateVar arg2 \code{prob} \tab numeric \tab probability of success. \cr
#' @templateVar arg3 \code{qprob} \tab numeric \tab probability of failure. \cr
#' @templateVar arg4 \code{mean} \tab numeric \tab location parameter. \cr
#' @templateVar arg5 \code{form} \tab character \tab form of negative binomial, see details. \cr
#' @templateVar constructorDets \code{size} as a positive whole number, and either \code{prob} or \code{qprob} as a number between 0 and 1, or \code{mean} as a numeric greater than the number of failures/successes (if form is 'tbf' or 'tbs'). These are related via, \deqn{qprob = 1 - prob} and the \code{mean} formula is dependent on the form. If \code{mean} is given then \code{qprob} and \code{prob} are ignored. If \code{qprob} is given then \code{prob} is ignored. \cr\cr The additional \code{form} argument determines which of the four Negative Binomial distributions should be constructed, this cannot be updated after construction. \code{form} should be one of "sbf" (successes before failures), "tbf" (trials before failures), "fbs" (failures before successes) or "tbs" (trials before successes). "fbs" is taken as default if none are supplied or an unrecognised form is given.
#' @templateVar additionalSeeAlso \code{\link{Binomial}} for the Binomial distribution and \code{\link{Geometric}} for the Geometric distribution.
#
#' @examples
#' # Different parameterisations
#' NegativeBinomial$new(size = 5, prob = 0.2)
#' NegativeBinomial$new(size = 5, qprob = 0.2)
#' NegativeBinomial$new(size = 5, mean = 4)
#'
#' # Different forms of the distribution
#' NegativeBinomial$new(form = "fbs")
#' NegativeBinomial$new(form = "sbf")
#'
#' # Use description to see which form is used
#' NegativeBinomial$new(form = "tbf")
#' NegativeBinomial$new(form = "tbs")
#'
#' x <- NegativeBinomial$new() # Default is size = 10, prob = 0.5 and failures before successes
#'
#' # Update parameters (form cannot be updated)
#' x$setParameterValue(qprob = 0.2) # When any parameter is updated, all others are too!
#' x$parameters()
#'
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
#' #
#' @export
NULL

NegativeBinomial <- R6Class("NegativeBinomial", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "NegativeBinomial",
    short_name = "NBinom",
    description = "Negative Binomial Probability Distribution.",
    packages = "stats",

    # Public methods
    # initialize
    initialize = function(size = 10, prob = 0.5, qprob = NULL, mean = NULL,
                          form = c("fbs", "sbf", "tbf", "tbs"),
                          decorators = NULL, verbose = FALSE) {

      form <- match.arg(form)

      private$.parameters <- getParameterSet(self, size, prob, qprob, mean, form, verbose)
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
    mean = function() {
      return(self$getParameterValue("mean"))
    },
    mode = function(which = NULL) {
      if (self$getParameterValue("form") == "sbf") {
        if (self$getParameterValue("size") <= 1) {
          return(0)
        } else {
          return(floor(((self$getParameterValue("size") - 1) * self$getParameterValue("prob")) / (self$getParameterValue("qprob"))))
        }
      } else if (self$getParameterValue("form") == "tbf") {
        if (self$getParameterValue("size") <= 1) {
          return(1)
        } else {
          return(floor(((self$getParameterValue("size") - 1) * self$getParameterValue("prob")) / (self$getParameterValue("qprob"))) + 10)
        }
      } else if (self$getParameterValue("form") == "fbs") {
        if (self$getParameterValue("size") <= 1) {
          return(0)
        } else {
          return(floor(((self$getParameterValue("size") - 1) * self$getParameterValue("qprob")) / (self$getParameterValue("prob"))))
        }
      } else {
        if (self$getParameterValue("size") <= 1) {
          return(1)
        } else {
          return(floor(((self$getParameterValue("size") - 1) * self$getParameterValue("qprob")) / (self$getParameterValue("prob"))) + 10)
        }
      }
    },
    variance = function() {
      if (self$getParameterValue("form") == "sbf" | self$getParameterValue("form") == "tbf") {
        return(self$getParameterValue("size") * self$getParameterValue("prob") / (self$getParameterValue("qprob")^2))
      } else if (self$getParameterValue("form") == "fbs" | self$getParameterValue("form") == "tbs") {
        return(self$getParameterValue("size") * self$getParameterValue("qprob") / (self$getParameterValue("prob")^2))
      }
    },
    skewness = function() {
      if (self$getParameterValue("form") == "sbf" | self$getParameterValue("form") == "tbf") {
        return((1 + self$getParameterValue("prob")) / sqrt(self$getParameterValue("size") * self$getParameterValue("prob")))
      } else {
        return((1 + self$getParameterValue("qprob")) / sqrt(self$getParameterValue("size") * self$getParameterValue("qprob")))
      }
    },
    kurtosis = function(excess = TRUE) {
      if (self$getParameterValue("form") == "sbf" | self$getParameterValue("form") == "tbf") {
        exkurtosis <- (self$getParameterValue("qprob")^2 - 6 * self$getParameterValue("qprob") + 6) /
          (self$getParameterValue("size") * self$getParameterValue("prob"))
      } else {
        exkurtosis <- (self$getParameterValue("prob")^2 - 6 * self$getParameterValue("prob") + 6) /
          (self$getParameterValue("size") * self$getParameterValue("qprob"))
      }

      if (excess) {
        return(exkurtosis)
      } else {
        return(exkurtosis + 3)
      }
    },
    mgf = function(t) {
      if (t < -log(self$getParameterValue("prob"))) {
        if (self$getParameterValue("form") == "sbf" | self$getParameterValue("form") == "tbf") {
          return((self$getParameterValue("qprob") / (1 - self$getParameterValue("prob") * exp(t)))^self$getParameterValue("size"))
        } else {
          return((self$getParameterValue("prob") / (1 - self$getParameterValue("qprob") * exp(t)))^self$getParameterValue("size"))
        }
      } else {
        return(NaN)
      }
    },
    cf = function(t) {
      if (self$getParameterValue("form") == "sbf" | self$getParameterValue("form") == "tbf") {
        return((self$getParameterValue("qprob") / (1 - self$getParameterValue("prob") * exp(t * 1i)))^self$getParameterValue("size"))
      } else {
        return((self$getParameterValue("prob") / (1 - self$getParameterValue("qprob") * exp(t * 1i)))^self$getParameterValue("size"))
      }
    },
    pgf = function(z) {
      if (abs(z) < 1 / self$getParameterValue("prob")) {
        if (self$getParameterValue("form") == "sbf") {
          return((self$getParameterValue("qprob") / (1 - self$getParameterValue("prob") * z))^self$getParameterValue("size"))
        } else if (self$getParameterValue("form") == "tbs") {
          return(((self$getParameterValue("prob") * z) / (1 - self$getParameterValue("qprob") * z))^self$getParameterValue("size"))
        } else if (self$getParameterValue("form") == "fbs") {
          return((self$getParameterValue("prob") / (1 - self$getParameterValue("qprob") * z))^self$getParameterValue("size"))
        } else if (self$getParameterValue("form") == "tbf") {
          return(((self$getParameterValue("qprob") * z) / (1 - self$getParameterValue("prob") * z))^self$getParameterValue("size"))
        }
      } else {
        return(NaN)
      }
    },

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      super$setParameterValue(..., lst = lst, error = error)
      if (self$getParameterValue("form") == "tbf" | self$getParameterValue("form") == "tbs") {
        private$.properties$support <- Interval$new(self$getParameterValue("size"), Inf, type = "[)", class = "integer")
      }
      invisible(self)
    }
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {

      pdf <- C_NegativeBinomialPdf(
        x = x,
        size = as.numeric(self$getParameterValue("size")),
        prob = as.numeric(self$getParameterValue("prob")),
        form = as.character(self$getParameterValue("form"))
      )

      if (ncol(pdf) == 1) {
        return(as.numeric(pdf))
      } else {
        return(pdf)
      }

    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      form <- self$getParameterValue("form")
      size <- self$getParameterValue("size")
      prob <- self$getParameterValue("prob")

      pnbinom = function(form, size, prob)
        if (form == "fbs") {
          return(call_C_base_pdqr(
            fun = "pnbinom",
            x = x,
            args = list(
              size = unlist(size),
              prob = unlist(prob)
            ),
            lower.tail = lower.tail,
            log = log.p,
            vec = test_list(size)
          ))
        } else if (form == "sbf") {

          return(1 - pbeta(self$getParameterValue("prob"), x + 1, self$getParameterValue("size")))
        } else {
          pdf_x <- self$workingSupport
          pdf_x <- seq.int(pdf_x$lower, pdf_x$upper)

          return(
            NumericCdf_Discrete(q = x,
                                x = pdf_x,
                                pdf = self$pdf(pdf_x),
                                lower = lower.tail,
                                logp = log.p)
          )
        }
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {
      if (self$getParameterValue("form") == "fbs") {
        size <- self$getParameterValue("size")
        prob <- self$getParameterValue("prob")
        return(call_C_base_pdqr(
          fun = "qnbinom",
          x = p,
          args = list(
            size = unlist(size),
            prob = unlist(prob)
          ),
          lower.tail = lower.tail,
          log = log.p,
          vec = test_list(size)
        ))
      } else {
        pdf_x <- self$workingSupport
        pdf_x <- seq.int(pdf_x$lower, pdf_x$upper)
        return(
          NumericQuantile(p = p,
                          x = pdf_x,
                          cdf = self$cdf(pdf_x),
                          lower = lower.tail,
                          logp = log.p)
        )
      }
    },
    .rand = function(n) {
      if (self$getParameterValue("form") == "fbs") {
        size <- self$getParameterValue("size")
        prob <- self$getParameterValue("prob")
        return(call_C_base_pdqr(
          fun = "rnbinom",
          x = n,
          args = list(
            size = unlist(size),
            prob = unlist(prob)
          ),
          vec = test_list(size)
        ))
      } else {
        x <- self$workingSupport
        x <- seq.int(x$lower, x$upper)
        return(sample(x, n, TRUE, self$pdf(x)))
      }
    },

    # getRefParams
    .getRefParams = function(paramlst) {
      lst <- list()
      if (!is.null(paramlst$size)) {
        lst <- c(lst, list(size = paramlst$size))
      } else {
        paramlst$size <- self$getParameterValue("size")
      }
      if (!is.null(paramlst$form)) stop("Distribution form cannot be changed after construction.")
      if (!is.null(paramlst$prob)) lst <- c(lst, list(prob = paramlst$prob))
      if (!is.null(paramlst$qprob)) lst <- c(lst, list(prob = 1 - paramlst$qprob))
      if (!is.null(paramlst$mean)) {
        if (self$getParameterValue("form") == "sbf") {
          lst <- c(lst, list(prob = paramlst$mean / (paramlst$size + paramlst$mean)))
        } else if (self$getParameterValue("form") == "tbf") {
          if (paramlst$mean <= paramlst$size) {
            stop("Mean must be > number of failures")
          }
          lst <- c(lst, list(prob = (paramlst$mean - paramlst$size) / paramlst$mean))
        } else if (self$getParameterValue("form") == "tbs") {
          if (paramlst$mean <= paramlst$size) {
            stop("Mean must be > number of successes")
          }
          lst <- c(lst, list(prob = paramlst$size / paramlst$mean))
        } else if (self$getParameterValue("form") == "fbs") {
          lst <- c(lst, list(prob = paramlst$size / (paramlst$mean + paramlst$size)))
        }
      }
      return(lst)
    },

    # traits
    .traits = list(variateForm = "univariate", valueSupport = "discrete"),

    .isCdf = FALSE,
    .isQuantile = FALSE,
    .isRand = FALSE
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "NBinom", ClassName = "NegativeBinomial",
    Type = "\u21150", ValueSupport = "discrete",
    VariateForm = "univariate",
    Package = "-"
  )
)
