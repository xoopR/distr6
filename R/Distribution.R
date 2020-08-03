#' @include distr6_globals.R helpers.R
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions.
#'
#' @name Distribution
#' @template param_decorators
#' @template method_setParameterValue
#' @template method_liesin
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#' @template param_n
#' @template param_paramid
#' @template class_distribution
#'
#' @return Returns R6 object of class Distribution.
#'
#' @export
Distribution <- R6Class("Distribution",
  lock_objects = FALSE,
  public = list(
    name = character(0),
    short_name = character(0),
    description = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param name `character(1)` \cr
    #' Full name of distribution.
    #' @param short_name `character(1)` \cr
    #' Short name of distribution for printing.
    #' @param type `([set6::Set])` \cr
    #' Distribution type.
    #' @param support `([set6::Set])` \cr
    #' Distribution support.
    #' @param symmetric `logical(1)` \cr
    #' Symmetry type of the distribution.
    #' @param pdf `function(1)` \cr
    #' Probability density function of the distribution. At least one of `pdf` and
    #' `cdf` must be provided.
    #' @param cdf `function(1)` \cr
    #' Cumulative distribution function of the distribution. At least one of `pdf` and
    #' `cdf` must be provided.
    #' @param quantile `function(1)` \cr
    #' Quantile (inverse-cdf) function of the distribution.
    #' @param rand `function(1)` \cr
    #' Simulation function for drawing random samples from the distribution.
    #' @param parameters `([ParameterSet])` \cr
    #' Parameter set for defining the parameters in the distribution, which should be set before
    #' construction.
    #' @param valueSupport `(character(1))` \cr
    #' The support type of the distribution, one of `"discrete", "continuous", "mixture"`.
    #' If `NULL`, determined automatically.
    #' @param variateForm `(character(1))` \cr
    #' The variate type of the distribution, one of `"univariate", "multivariate", "matrixvariate"`.
    #' If `NULL`, determined automatically.
    #' @param description `(character(1))` \cr
    #' Optional short description of the distribution.
    #' @param .suppressChecks `(logical(1))` \cr
    #' Used internally.
    initialize = function(name = NULL, short_name = NULL,
                          type, support = NULL,
                          symmetric = FALSE,
                          pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                          parameters = NULL, decorators = NULL, valueSupport = NULL,
                          variateForm = NULL, description = NULL,
                          .suppressChecks = FALSE) {

      if (.suppressChecks | inherits(self, "DistributionWrapper")) {

        if (!is.null(parameters)) parameters <- parameters$clone(deep = TRUE)
        if (!is.null(pdf)) formals(pdf) <- c(formals(pdf), list(self = self))
        if (!is.null(cdf)) formals(cdf) <- c(formals(cdf), list(self = self))
        if (!is.null(quantile)) formals(quantile) <- c(formals(quantile), list(self = self))
        if (!is.null(rand)) formals(rand) <- c(formals(rand), list(self = self))

      } else if (getR6Class(self) == "Distribution") {

        if (is.null(pdf) & is.null(cdf)) {
          stop("One of pdf or cdf must be provided.")
        }

        #------------
        # Name Checks
        #------------
        if (is.null(name) & is.null(short_name)) {
          checkmate::assert("One of 'name' or 'short_name' must be provided.")
        }
        if (is.null(short_name)) short_name <- gsub(" ", "", name, fixed = T)
        if (is.null(name)) name <- short_name
        checkmate::assertCharacter(c(name, short_name),
          .var.name = "'name' and 'short_name' must be of class 'character'."
        )
        checkmate::assert(length(strsplit(short_name, split = " ")[[1]]) == 1,
          .var.name = "'short_name' must be one word only."
        )

        #------------------------
        # Type and Support Checks
        #------------------------
        if (missing(type)) {
          stop("Distribution type must be provided.")
        }

        if (is.null(support)) support <- type
        assertSet(type)
        assertSet(support)

        #--------------------
        # valueSupport Checks
        #--------------------
        if (!is.null(valueSupport)) {
          valueSupport <- match.arg(valueSupport, c("continuous", "discrete", "mixture"))
        } else if (support$properties$countability == "uncountable") {
          valueSupport <- "continuous"
        } else {
          valueSupport <- "discrete"
        }

        #-------------------
        # variateForm Checks
        #-------------------
        if (!is.null(variateForm)) {
          variateForm <- match.arg(variateForm, c("univariate", "multivariate", "matrixvariate"))
        } else if (getR6Class(type) %in% c("ProductSet", "ExponentSet")) {
          variateForm <- "multivariate"
        } else {
          variateForm <- "univariate"
        }


        #-------------------
        # pdf and cdf Checks
        #-------------------
        if (!is.null(pdf)) {
          checkmate::assertSubset(names(formals(pdf)), c("x", "log"))
          formals(pdf) <- c(formals(pdf), list(self = self), alist(... = )) # nolint
        }

        if (!is.null(cdf)) {
          checkmate::assertSubset(names(formals(cdf)), c("x", "lower.tail", "log.p"))
          formals(cdf) <- c(formals(cdf), list(self = self), alist(... = )) # nolint
        }

        #-------------------------
        # quantile and rand Checks
        #-------------------------
        if (!is.null(quantile)) {
          checkmate::assertSubset(names(formals(quantile)), c("p", "lower.tail", "log.p"))
          formals(quantile) <- c(formals(quantile), list(self = self), alist(... = )) # nolint
        }

        if (!is.null(rand)) {
          stopifnot(names(formals(rand)) == "n")
          formals(rand) <- c(formals(rand), list(self = self), alist(... = )) # nolint
        }

        #-------------------------
        # Parameter Checks
        #-------------------------
        if (!is.null(parameters)) {
          checkmate::assertClass(parameters, "ParameterSet")
          # parameters <- parameters$clone(deep = TRUE)$.__enclos_env__$private$.update()
        }
      }

      if (!is.null(parameters)) {
        private$.parameters <- parameters
      }

      if (!is.null(pdf)) {
        private$.pdf <- pdf
        private$.isPdf <- 1L
      }
      if (!is.null(cdf)) {
        private$.cdf <- cdf
        private$.isCdf <- 1L
      }
      if (!is.null(quantile)) {
        private$.quantile <- quantile
        private$.isQuantile <- 1L
      }
      if (!is.null(rand)) {
        private$.rand <- rand
        private$.isRand <- 1L
      }

      if (!is.null(name)) self$name <- name
      if (!is.null(short_name)) self$short_name <- short_name

      private$.properties$support <- support
      private$.traits$type <- type
      private$.traits$valueSupport <- valueSupport
      private$.traits$variateForm <- variateForm

      if (!is.null(description)) self$description <- description

      symm <- ifelse(symmetric, "symmetric", "asymmetric")
      private$.properties$symmetry <- symm

      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }

      lockBinding("name", self)
      lockBinding("short_name", self)
      lockBinding("description", self)
      lockBinding("traits", self)
      lockBinding("parameters", self)
      invisible(self)
    },

    #' @description
    #' Printable string representation of the `Distribution`. Primarily used internally.
    #' @param n `(integer(1))` \cr
    #' Number of parameters to display when printing.
    strprint = function(n = 2) {
      if (length(private$.parameters) != 0) {
        p <- as.data.table(self$parameters())
        settable <- p$settable
        id <- p[settable, "id"][[1]]
        value <- p[settable, "value"][[1]]
        lng <- length(id)
        if (lng > (2 * n)) {
          string <- paste0(
            self$short_name, "(", paste(id[1:n], value[1:n], sep = " = ", collapse = ", "),
            ",...,", paste(id[(lng - n + 1):lng], value[(lng - n + 1):lng],
              sep = " = ",
              collapse = ", "
            ), ")"
          )
        } else {
          string <- paste0(
            self$short_name, "(", paste(id, value, sep = " = ", collapse = ", "),
            ")"
          )
        }
      } else {
        string <- paste0(self$short_name)
      }
      return(string)
    },

    #' @description
    #' Prints the `Distribution`.
    #' @param n `(integer(1))` \cr
    #' Passed to `$strprint`.
    #' @param ... `ANY` \cr
    #' Unused. Added for consistency.
    print = function(n = 2, ...) {
      cat(self$strprint(n = n), "\n")
      invisible(self)
    },

    #' @description
    #' Prints a summary of the `Distribution`.
    #' @param full `(logical(1))` \cr
    #' If `TRUE` (default) prints a long summary of the distribution,
    #' otherwise prints a shorter summary.
    #' @param ... `ANY` \cr
    #' Unused. Added for consistency.
    summary = function(full = TRUE, ...) {

      if (full) {

        settable <- as.data.table(self$parameters())$settable
        name <- ifelse(is.null(self$description), self$name, self$description)
        if (!any(settable)) {
          cat(name)
        } else {
          cat(name, "Parameterised with:\n")

          cat(" ", paste(unlist(as.data.table(self$parameters())[settable, "id"]),
                         unlist(as.data.table(self$parameters())[settable, "value"]),
                         sep = " = ", collapse = ", "
          ))
        }

        a_exp <- suppressMessages(try(self$mean(), silent = T))
        a_var <- suppressMessages(try(self$variance(), silent = T))
        a_skew <- suppressMessages(try(self$skewness(), silent = T))
        a_kurt <- suppressMessages(try(self$kurtosis(), silent = T))

        if (!inherits(a_exp, "try-error") | !inherits(a_var, "try-error") |
          !inherits(a_skew, "try-error") | !inherits(a_kurt, "try-error")) {
          cat("\n\n ", "Quick Statistics", "\n")
        }

        if (!inherits(a_exp, "try-error")) {
          cat("\tMean:")
          if (length(a_exp) > 1) {
            cat("\t\t", paste0(a_exp, collapse = ", "), "\n", sep = "")
          } else {
            cat("\t\t", a_exp, "\n", sep = "")
          }
        }
        if (!inherits(a_var, "try-error")) {
          cat("\tVariance:")
          if (length(a_var) > 1) {
            cat("\t", paste0(a_var, collapse = ", "), "\n", sep = "")
          } else {
            cat("\t", a_var, "\n", sep = "")
          }
        }
        if (!inherits(a_skew, "try-error")) {
          cat("\tSkewness:")
          # if(length(a_skew) > 1)
          #   cat("\t", paste0(a_skew, collapse = ", "),"\n", sep = "")
          # else
          cat("\t", a_skew, "\n", sep = "")
        }
        if (!inherits(a_kurt, "try-error")) {
          cat("\tEx. Kurtosis:")
          # if(length(a_kurt) > 1)
          #   cat("\t", paste0(a_kurt, collapse = ", "),"\n", sep = "")
          # else
          cat("\t", a_kurt, "\n", sep = "")
        }
        cat("\n")

        cat(
          " Support:", self$properties$support$strprint(), "\tScientific Type:",
          self$traits$type$strprint(), "\n"
        )
        cat("\n Traits:\t", self$traits$valueSupport, "; ", self$traits$variateForm, sep = "")
        cat("\n Properties:\t", self$properties$symmetry, sep = "")
        if (!inherits(a_kurt, "try-error")) cat(";", exkurtosisType(a_kurt))
        if (!inherits(a_skew, "try-error")) cat(";", skewType(a_skew))

        if (length(self$decorators) != 0) {
          cat("\n\n Decorated with: ", paste0(self$decorators, collapse = ", "))
        }

      } else {
        if (length(private$.parameters) != 0) {
          cat(self$strprint())
        } else {
          cat(self$name)
        }
        cat("\nScientific Type:", self$traits$type$strprint(), "\t See $traits for more")
        cat("\nSupport:", self$properties$support$strprint(), "\t See $properties for more")
      }
      cat("\n")
      invisible(self)
    },

    #' @description
    #' Returns the full parameter details for the supplied parameter.
    parameters = function(id = NULL) {
      if (length(private$.parameters) == 0) {
        return(NULL)
      } else {
        return(private$.parameters$parameters(id))
      }
    },

    #' @description
    #' Returns the value of the supplied parameter.
    getParameterValue = function(id, error = "warn") {
      if (length(private$.parameters) == 0) {
        return(NULL)
      } else {
        return(private$.parameters$getParameterValue(id, error))
      }
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    #'
    #' @examples
    #' b = Binomial$new()
    #' b$setParameterValue(size = 4, prob = 0.4)
    #' b$setParameterValue(lst = list(size = 4, prob = 0.4))
    setParameterValue = function(..., lst = NULL, error = "warn") {
      if (is.null(lst)) {
        lst <- list(...)
      }
      if (private$.parameters$length && length(lst)) {
        self$parameters()$setParameterValue(lst = lst, error = error)
      }
      invisible(self)
    },

    #' @description
    #'  For discrete distributions the probability mass function (pmf) is returned, defined as
    #'  \deqn{p_X(x) = P(X = x)}
    #'  for continuous distributions the probability density function (pdf), \eqn{f_X}, is returned
    #'  \deqn{f_X(x) = P(x < X \le x + dx)}
    #'  for some infinitesimally small \eqn{dx}.
    #'
    #' If available a pdf will be returned using an analytic expression. Otherwise,
    #' if the distribution has not been decorated with [FunctionImputation], \code{NULL} is
    #' returned.
    #'
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #'
    #' @examples
    #' b <- Binomial$new()
    #' b$pdf(1:10)
    #' b$pdf(1:10, log = TRUE)
    #' b$pdf(data = matrix(1:10))
    #'
    #' mvn <- MultivariateNormal$new()
    #' mvn$pdf(1, 2)
    #' mvn$pdf(1:2, 3:4)
    #' mvn$pdf(data = matrix(1:4, nrow = 2), simplify = FALSE)
    pdf = function(..., log = FALSE, simplify = TRUE, data = NULL) {

      if (!private$.isPdf) {
        return(NULL)
      }

      if (is.null(data)) {
        if (!...length()) {
          return(NULL)
        } else if (!length(...elt(1))) {
          return(NULL)
        }
      }

      data <- pdq_point_assert(..., self = self, data = data)
      if (inherits(data, "matrix")) {
        if (!self$liesInType(apply(data, 1, as.Tuple), all = TRUE, bound = TRUE)) {
          stop(
            sprintf("Not all points in %s lie in the distribution domain (%s).",
                strCollapse(apply(data, 1, function(x) as.Tuple(x)$strprint())),
                self$traits$type$strprint())
          )
        }
      } else {
        if (!suppressWarnings(self$liesInType(as.numeric(data), all = TRUE, bound = TRUE))) {
          stop(
            sprintf("Not all points in %s lie in the distribution domain (%s).",
                  strCollapse(as.numeric(data)), self$traits$type$strprint())
          )
        }
      }


      if (log) {
        if (private$.log) {
          pdqr <- private$.pdf(data, log = log)
        } else {
          if (!("CoreStatistics" %in% self$decorators)) {
            stop("No analytical method for log available.
Use CoreStatistics decorator to numerically estimate this.")
          } else {
            pdqr <- log(private$.pdf(data))
          }
        }
      } else {
        pdqr <- private$.pdf(data)
      }

      pdqr_returner(pdqr, simplify, self$short_name)
    },

    #' @description
    #' The (lower tail) cumulative distribution function, \eqn{F_X}, is defined as
    #'  \deqn{F_X(x) = P(X \le x)}
    #'  If \code{lower.tail} is FALSE then \eqn{1 - F_X(x)} is returned, also known as the
    #'  \code{\link{survival}} function.
    #'
    #' If available a cdf will be returned using an analytic expression. Otherwise,
    #' if the distribution has not been decorated with [FunctionImputation], \code{NULL} is
    #' returned.
    #'
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #'
    #' @examples
    #' b <- Binomial$new()
    #' b$cdf(1:10)
    #' b$cdf(1:10, log.p = TRUE, lower.tail = FALSE)
    #' b$cdf(data = matrix(1:10))
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {

      if (!private$.isCdf) {
        return(NULL)
      }

      if (is.null(data)) {
        if (!...length()) {
          return(NULL)
        } else if (!length(...elt(1))) {
          return(NULL)
        }
      }

      data <- pdq_point_assert(..., self = self, data = data)
      if (inherits(data, "matrix")) {
        if (!self$liesInType(apply(data, 1, as.Tuple), all = TRUE, bound = TRUE)) {
          stop(
            sprintf("Not all points in %s lie in the distribution domain (%s).",
                    strCollapse(apply(data, 1, function(x) as.Tuple(x)$strprint())),
                    self$traits$type$strprint())
          )
        }
      } else {
        if (!suppressWarnings(self$liesInType(as.numeric(data), all = TRUE, bound = TRUE))) {
          stop(
            sprintf("Not all points in %s lie in the distribution domain (%s).",
                    strCollapse(as.numeric(data)), self$traits$type$strprint())
          )
        }
      }

      if (log.p | !lower.tail) {
        if (private$.log) {
          pdqr <- private$.cdf(data, log.p = log.p, lower.tail = lower.tail)
        } else {
          if (!("CoreStatistics" %in% self$decorators)) {
            stop("No analytical method for log.p or lower.tail available. Use CoreStatistics
decorator to numerically estimate this.")
          } else {
            pdqr <- private$.cdf(data)
            if (!lower.tail) pdqr <- 1 - pdqr
            if (log.p) pdqr <- log(pdqr)
          }
        }
      } else {
        pdqr <- private$.cdf(data)
      }

      pdqr_returner(pdqr, simplify, self$short_name)
    },

    #' @description
    #'  The quantile function, \eqn{q_X}, is the inverse cdf, i.e.
    #'  \deqn{q_X(p) = F^{-1}_X(p) = \inf\{x \in R: F_X(x) \ge p\}}{q_X(p) = F^(-1)_X(p) = inf{x \epsilon R: F_X(x) \ge p}} #nolint
    #'
    #'  If \code{lower.tail} is FALSE then \eqn{q_X(1-p)} is returned.
    #'
    #' If available a quantile will be returned using an analytic expression. Otherwise,
    #' if the distribution has not been decorated with [FunctionImputation], \code{NULL} is
    #' returned.
    #'
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #'
    #' @examples
    #' b <- Binomial$new()
    #' b$quantile(0.42)
    #' b$quantile(log(0.42), log.p = TRUE, lower.tail = TRUE)
    #' b$quantile(data = matrix(c(0.1,0.2)))
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {

      if (!private$.isQuantile) {
        return(NULL)
      }

      if (is.null(data)) {
        if (!...length()) {
          return(NULL)
        } else if (!length(...elt(1))) {
            return(NULL)
        }
      }

      data <- pdq_point_assert(..., self = self, data = data)

      if (!log.p) {
        if (!Interval$new(0, 1)$contains(as.numeric(data), all = TRUE)) {
          stop(sprintf("Not all points in %s lie in [0, 1].", strCollapse(as.numeric(data))))
        }
      } else {
        if (!Interval$new(0, 1)$contains(as.numeric(exp(data)), all = TRUE)) {
          stop(sprintf("Not all points in %s lie in [0, 1].", strCollapse(as.numeric(exp(data)))))
        }
      }

      if (log.p | !lower.tail) {
        if (private$.log) {
          pdqr <- private$.quantile(data, log.p = log.p, lower.tail = lower.tail)
        } else {
          if (!("CoreStatistics" %in% self$decorators)) {
            stop("No analytical method for log.p or lower.tail available. Use CoreStatistics
decorator to numerically estimate this.")
          } else {
            if (log.p) data <- exp(data)
            if (!lower.tail) data <- 1 - data
            pdqr <- private$.quantile(data)
          }
        }
      } else {
        pdqr <- private$.quantile(data)
      }

      pdqr_returner(pdqr, simplify, self$short_name)
    },

    #' @description
    #' The rand function draws `n` simulations from the distribution.
    #'
    #' If available simulations will be returned using an analytic expression. Otherwise,
    #' if the distribution has not been decorated with [FunctionImputation], \code{NULL} is
    #' returned.
    #'
    #' @examples
    #' b <- Binomial$new()
    #' b$rand(10)
    #'
    #' mvn <- MultivariateNormal$new()
    #' mvn$rand(5)
    rand = function(n, simplify = TRUE) {

      if (missing(n) | private$.isRand == 0L) {
        return(NULL)
      }

      if (length(n) > 1) {
        n <- length(n)
      }

      pdqr <- private$.rand(n)

      pdqr_returner(pdqr, simplify, self$short_name)
    },

    #' @description
    #' Returns the precision of the distribution as `1/self$variance()`.
    prec = function() {
      return(1 / self$variance())
    },

    #' @description
    #' Returns the standard deviation of the distribution as `sqrt(self$variance())`.
    stdev = function() {
      return(sqrt(self$variance()))
    },

    #' @description
    #' Returns the median of the distribution. If an analytical expression is available
    #' returns distribution median, otherwise if symmetric returns `self$mean`, otherwise
    #' returns `self$quantile(0.5)`.
    #' @param na.rm `(logical(1))`\cr
    #' Ignored, addded for consistency.
    #' @param ... `ANY` \cr
    #' Ignored, addded for consistency.
    median = function(na.rm = NULL, ...) {
      if (testSymmetric(self)) {
        med <- try(self$mean(), silent = TRUE)
        if (class(med) == "try-error") {
          return(self$quantile(0.5))
        } else if (is.null(med)) {
          return(self$quantile(0.5))
        } else {
          return(med)
        }
      } else if (!testUnivariate(self)) {
        return(NaN)
      } else {
        return(self$quantile(0.5))
      }
    },

    #' @description
    #' Inter-quartile range of the distribution. Estimated as
    #' `self$quantile(0.75) - self$quantile(0.25)`.
    iqr = function() {
      return(self$quantile(0.75) - self$quantile(0.25))
    },

    #' @description
    #' If univariate returns `1`, otherwise returns the distribution correlation.
    correlation = function() {
      if (testUnivariate(self)) {
        return(1)
      } else {
        return(self$variance() / (sqrt(diag(self$variance()) %*% t(diag(self$variance())))))
      }
    },

    #' @description
    #' Tests if the given values lie in the support of the distribution.
    #' Uses `[set6::Set]$contains`.
    liesInSupport = function(x, all = TRUE, bound = FALSE) {
      return(self$properties$support$contains(x, all, bound))
    },

    #' @description
    #' Tests if the given values lie in the type of the distribution.
    #' Uses `[set6::Set]$contains`.
    liesInType = function(x, all = TRUE, bound = FALSE) {
      return(self$traits$type$contains(x, all, bound))
    },

    #' @description
    #' Returns an estimate for the computational support of the distribution.
    #' If an analytical cdf is available, then this is computed as the smallest interval
    #' in which the cdf lower bound is `0` and the upper bound is `1`, bounds are incremented in
    #' 10^i intervals. If no analytical cdf is available, then this is computed as the smallest
    #' interval in which the lower and upper bounds of the pdf are `0`, this is much less precise
    #' and is more prone to error. Used primarily by decorators.
    workingSupport = function() {

      if (testCountablyFinite(self$properties$support)) {
        return(self$properties$support)
      }

      lower <- self$inf
      upper <- self$sup

      if (lower == -Inf) {
        if (private$.isCdf == 1L) {
          for (i in -c(0, 10^(1:1000))) { # nolint
            if (i >= lower & i <= upper) {
              if (self$cdf(i) == 0) {
                lower <- i
                break() # nolint
              }
            }
          }
        } else {
          for (i in -c(0, 10^(1:1000))) { # nolint
            if (i >= lower & i <= upper) {
              if (self$pdf(i) == 0) {
                lower <- i
                break() # nolint
              }
            }
          }
        }

      }

      if (upper == Inf) {
        if (private$.isCdf == 1L) {
          for (i in c(0, 10^(1:1000))) { # nolint
            if (i >= lower & i <= upper) {
              if (self$cdf(i) == 1) {
                upper <- i
                break() # nolint
              }
            }
          }
        } else {
          for (i in c(0, 10^(1:1000))) { # nolint
            if (i >= lower & i <= upper) {
              if (self$pdf(i) == 0) {
                upper <- i
                break() # nolint
              }
            }
          }
        }
      }

      class <- if (testDiscrete(self)) "integer" else "numeric"

      Interval$new(lower, upper, class = class)
    }
  ),

  active = list(
    #' @field decorators
    #' Returns decorators currently used to decorate the distribution.
    decorators = function() {
      return(private$.decorators)
    },

    #' @field traits
    #' Returns distribution traits.
    traits = function() {
      return(private$.traits)
    },

    #' @field valueSupport
    #' Deprecated, use `$traits$valueSupport`.
    valueSupport = function() {
      message("Deprecated. Use $traits$valueSupport instead.")
      return(self$traits$valueSupport)
    },

    #' @field variateForm
    #' Deprecated, use `$traits$variateForm`.
    variateForm = function() {
      message("Deprecated. Use $traits$variateForm instead.")
      return(self$traits$variateForm)
    },

    #' @field type
    #' Deprecated, use `$traits$type`.
    type = function() {
      message("Deprecated. Use $traits$type instead.")
      return(self$traits$type)
    },

    #' @field properties
    #' Returns distribution properties, including skewness type and symmetry.
    properties = function() {
      private$.properties
    },

    #' @field support
    #' Deprecated, use `$properties$type`.
    support = function() {
      message("Deprecated. Use $properties$support instead.")
      return(self$properties$support)
    },

    #' @field symmetry
    #' Deprecated, use `$properties$symmetry`.
    symmetry = function() {
      message("Deprecated. Use $properties$symmetry instead.")
      return(self$properties$symmetry)
    },

    #' @field sup
    #' Returns supremum (upper bound) of the distribution support.
    sup = function() {
      return(self$properties$support$upper)
    },

    #' @field inf
    #' Returns infimum (lower bound) of the distribution support.
    inf = function() {
      return(self$properties$support$lower)
    },

    #' @field dmax
    #' Returns maximum of the distribution support.
    dmax = function() {
      return(self$properties$support$max)
    },

    #' @field dmin
    #' Returns minimum of the distribution support.
    dmin = function() {
      return(self$properties$support$min)
    },

    #' @field kurtosisType
    #' Deprecated, use `$properties$kurtosis`.
    kurtosisType = function() {
      message("Deprecated. Use $properties$kurtosis instead.")
      return(self$properties$kurtosis)
    },

    #' @field skewnessType
    #' Deprecated, use `$properties$skewness`.
    skewnessType = function() {
      message("Deprecated. Use $properties$skewness instead.")
      return(self$properties$skewness)
    }
  ),

  private = list(
    .parameters = NULL,
    .decorators = NULL,
    .properties = list(),
    .traits = NULL,
    .variates = 1,
    .isPdf = 0L,
    .isCdf = 0L,
    .isQuantile = 0L,
    .isRand = 0L,
    .log = FALSE,
    .updateDecorators = function(decs) {
      private$.decorators <- decs
    }
  )
)


#' @title String Representation of Print
#' @name strprint
#' @description Parsable string to be supplied to \code{print}, \code{data.frame}, etc.
#' @details strprint is a suggested method that should be included in all R6 classes to be passed to
#' methods such as \code{cat}, \code{summary} and \code{print}. Additionally can be used to easily
#' parse R6 objects into data-frames, see examples.
#'
#' @param object R6 object
#' @param n Number of parameters to display before & after ellipsis
#' @usage strprint(object, n = 2)
#'
#' @return String representation of the distribution.
#'
#' @examples
#' Triangular$new()$strprint()
#' Triangular$new()$strprint(1)
#' @export
NULL

#' @title Distribution Summary
#' @description Summary method for distribution objects (and all child classes).
#'
#' @section R6 Usage: $summary(full = TRUE)
#' @param object Distribution.
#' @param full logical; if TRUE (default), gives an extended summary, otherwise brief.
#' @param ... additional arguments.
#'
#' @seealso \code{\link{Distribution}}
#'
#' @return Printed summary of the distribution.
#'
#' @export
summary.Distribution <- function(object, full = TRUE, ...) {}

#' @name pdf
#' @title Probability Density Function
#' @description See [Distribution]`$pdf`
#' @usage pdf(object, ..., log = FALSE, simplify = TRUE, data = NULL)
#' @param object ([Distribution])
#' @param ... `(numeric())` \cr
#' Points to evaluate the probability density function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param log `logical(1)` \cr
#' If `TRUE` returns log-pdf. Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Pdf evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a [data.table::data.table].
#'
#' @export
NULL

#' @name cdf
#' @title Cumulative Distribution Function
#' @description See [Distribution]`$cdf`
#' @usage cdf(object, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL)
#' @param object ([Distribution])
#' @param ... `(numeric())` \cr
#' Points to evaluate the cumulative distribution function of the distribution. Arguments do not
#' need to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param lower.tail `logical(1)` \cr
#' If `TRUE` (default), probabilities are `X <= x`, otherwise, `X > x`.
#' @param log.p `logical(1)` \cr
#' If `TRUE` returns log-cdf. Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Cdf evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a [data.table::data.table].
#'
#' @export
NULL

#' @title Inverse Cumulative Distribution Function
#' @description See [Distribution]`$quantile`
#' @importFrom stats quantile
#' @param x ([Distribution])
#' @param ... `(numeric())` \cr
#' Points to evaluate the quantile function of the distribution. Arguments do not need
#' to be named. The length of each argument corresponds to the number of points to evaluate,
#' the number of arguments corresponds to the number of variables in the distribution.
#' See examples.
#' @param lower.tail `logical(1)` \cr
#' If `TRUE` (default), probabilities are `X <= x`, otherwise, `X > x`.
#' @param log.p `logical(1)` \cr
#' If `TRUE` returns log-cdf. Default is `FALSE`.
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#' @param data [array] \cr
#' Alternative method to specify points to evaluate. If univariate then rows correspond with number
#' of points to evaluate and columns correspond with number of variables to evaluate. In the special
#' case of [VectorDistribution]s of multivariate distributions, then the third dimension corresponds
#' to the distribution in the vector to evaluate.
#'
#' @return Quantile evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a [data.table::data.table].
#'
#' @export
quantile.Distribution <- function(x, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE,
                                  data = NULL) {}

#' @name rand
#' @title Random Simulation Function
#' @description See [Distribution]`$rand`
#' @usage rand(object, n, simplify = TRUE)
#' @param object ([Distribution])
#' @param n `(numeric(1))` \cr
#' Number of points to simulate from the distribution. If length greater than \eqn{1}, then
#' `n <- length(n)`,
#' @param simplify `logical(1)` \cr
#' If `TRUE` (default) simplifies the pdf if possible to a `numeric`, otherwise returns a
#' [data.table::data.table][data.table].
#'
#' @return Simulations as either a numeric if \code{simplify} is TRUE
#' or as a [data.table::data.table].
#'
#' @export
NULL

#' @name prec
#' @title Precision of a Distribution
#' @description Precision of a distribution assuming variance is provided.
#' @usage prec(object)
#' @param object Distribution.
#' @return Reciprocal of variance as a numeric.
#' @export
NULL

#' @name stdev
#' @title Standard Deviation of a Distribution
#' @description Standard deviation of a distribution assuming variance is provided.
#'
#' @usage stdev(object)
#' @param object Distribution.
#' @return Square-root of variance as a numeric.
#'
#' @export
NULL

#' @title Median of a Distribution
#' @description Median of a distribution assuming quantile is provided.
#'
#' @importFrom stats median
#' @method median Distribution
#' @param x Distribution.
#' @param na.rm ignored, added for consistency with S3 generic.
#' @param ... ignored, added for consistency with S3 generic.
#' @return Quantile function evaluated at 0.5 as a numeric.
#' @export
median.Distribution <- function(x, na.rm = NULL, ...) {}

#' @title Distribution Interquartile Range
#' @name iqr
#' @description Interquartile range of a distribution
#' @usage iqr(object)
#' @param object Distribution.
#' @return Interquartile range of distribution as a numeric.
#' @export
NULL

#' @title Distribution Correlation
#' @name correlation
#' @description Correlation of a distribution.
#'
#' @usage correlation(object)
#'
#' @param object Distribution.
#'
#' @return Either '1' if distribution is univariate or the correlation as a numeric or matrix.
#'
#' @export
NULL

#' @name liesInSupport
#' @title Test if Data Lies in Distribution Support
#' @description Tests if the given data lies in the support of the Distribution, either tests if all
#' data lies in the support or any of it.
#'
#' @usage liesInSupport(object, x, all = TRUE, bound = FALSE)
#' @param object Distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) uses dmin/dmax otherwise inf/sup.
#'
#' @return Either a vector of logicals if \code{all} is FALSE otherwise returns TRUE if every
#' element lies in the distribution support or FALSE otherwise.
#'
#' @export
NULL

#' @name liesInType
#' @title Test if Data Lies in Distribution Type
#' @description Tests if the given data lies in the type of the Distribution, either tests if all
#' data lies in the type or any of it.
#'
#' @usage liesInType(object, x, all = TRUE, bound = FALSE)
#' @param object Distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) uses dmin/dmax otherwise inf/sup.
#'
#' @return Either a vector of logicals if \code{all} is FALSE otherwise returns TRUE if every
#' element lies in the distribution type or FALSE otherwise.
#'
#' @export
NULL


#' @name decorators
#' @title Decorators Accessor
#' @usage decorators(object)
#' @section R6 Usage: $decorators
#' @param object Distribution.
#' @description Returns the decorators added to a distribution.
#' @return Character vector of decorators.
#' @export
NULL

#' @name traits
#' @title Traits Accessor
#' @usage traits(object)
#' @section R6 Usage: $traits
#' @param object Distribution.
#' @description Returns the traits of the distribution.
#' @return List of traits.
#' @export
NULL

#' @name valueSupport
#' @title Value Support Accessor - Deprecated
#' @usage valueSupport(object)
#' @param object Distribution.
#' @description Deprecated. Use $traits$valueSupport
#' @return One of "discrete"/"continuous"/"mixture".
#' @export
NULL

#' @name variateForm
#' @title Variate Form Accessor - Deprecated
#' @usage variateForm(object)
#' @param object Distribution.
#' @description Deprecated. Use $traits$variateForm
#' @return One of "univariate"/"multivariate"/"matrixvariate".
#' @export
NULL

#' @name type
#' @title Type Accessor - Deprecated
#' @usage type(object)
#' @section R6 Usage: $type
#' @param object Distribution.
#' @description Deprecated. Use $traits$type
#' @return An R6 object of class [set6::Set].
#' @export
NULL

#' @name properties
#' @title Properties Accessor
#' @usage properties(object)
#' @section R6 Usage: $properties
#' @param object Distribution.
#' @description Returns the properties of the distribution.
#' @return List of distribution properties.
#' @export
NULL

#' @name support
#' @title Support Accessor - Deprecated
#' @usage support(object)
#' @section R6 Usage: $support
#' @param object Distribution.
#' @description Deprecated. Use $properties$support
#' @details The support of a probability distribution is defined as the interval where the pmf/pdf
#' is greater than zero,
#' \deqn{Supp(X) = \{x \ \in R: \ f_X(x) \ > \ 0\}}{Supp(X) = {x \epsilon R: f_X(x) > 0}}
#' where \eqn{f_X} is the pmf if distribution \eqn{X} is discrete, otherwise the pdf.
#' @return An R6 object of class [set6::Set].
#' @export
NULL

#' @name symmetry
#' @title Symmetry Accessor - Deprecated
#' @usage symmetry(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$symmetry.
#' @return One of "symmetric" or "asymmetric".
#' @export
NULL

#' @name sup
#' @title Supremum Accessor
#' @usage sup(object)
#' @section R6 Usage: $sup
#' @param object Distribution.
#' @description Returns the distribution supremum as the supremum of the support.
#' @return Supremum as a numeric.
#' @export
NULL

#' @name inf
#' @title Infimum Accessor
#' @usage inf(object)
#' @section R6 Usage: $inf
#' @param object Distribution.
#' @description Returns the distribution infimum as the infimum of the support.
#' @return Infimum as a numeric.
#' @export
NULL

#' @name dmax
#' @title Distribution Maximum Accessor
#' @usage dmax(object)
#' @section R6 Usage: $dmax
#' @param object Distribution.
#' @description Returns the distribution maximum as the maximum of the support. If the support is
#' not bounded above then maximum is given by
#' \deqn{maximum = supremum - 1.1e-15}
#' @return Maximum as a numeric.
#' @seealso \code{\link{support}}, \code{\link{dmin}}, \code{\link{sup}}, \code{\link{inf}}
#' @export
NULL

#' @name dmin
#' @title Distribution Minimum Accessor
#' @usage dmin(object)
#' @section R6 Usage: $dmin
#' @param object Distribution.
#' @description Returns the distribution minimum as the minimum of the support. If the support is
#' not bounded below then minimum is given by
#' \deqn{minimum = infimum + 1.1e-15}
#' @return Minimum as a numeric.
#' @export
NULL

#' @name kurtosisType
#' @title Type of Kurtosis Accessor - Deprecated
#' @usage kurtosisType(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$kurtosis.
#' @return If the distribution kurtosis is present in properties, returns one of
#' "platykurtic"/"mesokurtic"/"leptokurtic", otherwise returns NULL.
#' @export
NULL

#' @name skewnessType
#' @title Type of Skewness Accessor - Deprecated
#' @usage skewnessType(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$skewness.
#' @return If the distribution skewness is present in properties, returns one of
#' "negative skew", "no skew", "positive skew", otherwise returns NULL.
#' @export
NULL

#' @name workingSupport
#' @title Approximate Finite Support
#' @usage workingSupport(object)
#' @param object Distribution.
#' @description If the distribution has an infinite support then this function calculates
#' the approximate finite limits by finding the largest small number for which `cdf == 0` and the
#' smallest large number for which `cdf == 1`.
#' @return \CRANpkg{set6} object.
#' @export
NULL
