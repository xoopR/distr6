#' @title Imputed Pdf/Cdf/Quantile/Rand Functions Decorator
#'
#' @description This decorator imputes missing pdf/cdf/quantile/rand methods from R6 Distributions
#' by using strategies dependent on which methods are already present in the distribution. Unlike
#' other decorators, private methods are added to the [Distribution], not public methods.
#' Therefore the underlying public `[Distribution]$pdf`, `[Distribution]$pdf`,
#' `[Distribution]$quantile`, and `[Distribution]$rand` functions stay the same.
#'
#' @template class_decorator
#' @template field_packages
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#' @template param_n
#' @template method_decorate
#'
#' @examples
#' if (requireNamespace("GoFKernel", quietly = TRUE) &&
#'     requireNamespace("pracma", quietly = TRUE)) {
#' pdf <- function(x) ifelse(x < 1 | x > 10, 0, 1 / 10)
#'
#' x <- Distribution$new("Test",
#'   pdf = pdf,
#'   support = set6::Interval$new(1, 10, class = "integer"),
#'   type = set6::Naturals$new()
#' )
#' decorate(x, "FunctionImputation", n = 1000)
#'
#' x <- Distribution$new("Test",
#'   pdf = pdf,
#'   support = set6::Interval$new(1, 10, class = "integer"),
#'   type = set6::Naturals$new(),
#'   decorators = "FunctionImputation"
#' )
#'
#' x <- Distribution$new("Test",
#'   pdf = pdf,
#'   support = set6::Interval$new(1, 10, class = "integer"),
#'   type = set6::Naturals$new()
#' )
#' FunctionImputation$new()$decorate(x, n = 1000)
#'
#' x$pdf(1:10)
#' x$cdf(1:10)
#' x$quantile(0.42)
#' x$rand(4)
#' }
#' @export
FunctionImputation <- R6Class("FunctionImputation",
  inherit = DistributionDecorator,
  public = list(
    packages = c("pracma", "GoFKernel"),

    #' @description
    #' Decorates the given distribution with the methods available in this decorator.
    #' @param n `(integer(1))`\cr
    #' Grid size for imputing functions, cannot be changed after decorating.
    #' Generally larger `n` means better accuracy but slower computation, and smaller `n`
    #' means worse accuracy and faster computation.
    decorate = function(distribution, n = 1000) {

      assert_pkgload(self$packages)

      if (!testUnivariate(distribution)) {
        stop("FunctionImputation is currently only supported for univariate distributions.")
      }

      if ("FunctionImputation" %in% distribution$decorators) {
        message(paste(distribution$name, "is already decorated with FunctionImputation."))
        invisible(self)
      } else {
        pdist <- distribution$.__enclos_env__$private
        pdist$.log <- TRUE
        pdist$n_grid <- checkmate::assertIntegerish(n)

        if (!isPdf(distribution)) {
          pdf <- FunctionImputation$private_methods$.pdf
          formals(pdf) <- c(formals(pdf), list(self = distribution, private = pdist))
          pdist$.pdf <- pdf
          pdist$.isPdf <- -1L
        }

        if (!isCdf(distribution)) {
          cdf <- FunctionImputation$private_methods$.cdf
          formals(cdf) <- c(formals(cdf), list(self = distribution, private = pdist))
          pdist$.cdf <- cdf
          pdist$.isCdf <- -1L
        }

        if (!isQuantile(distribution)) {
          quantile <- FunctionImputation$private_methods$.quantile
          formals(quantile) <- c(formals(quantile), list(self = distribution, private = pdist))
          pdist$.quantile <- quantile
          pdist$.isQuantile <- -1L
        }

        if (!isRand(distribution)) {
          rand <- FunctionImputation$private_methods$.rand
          formals(rand) <- c(formals(rand), list(self = distribution, private = pdist))
          pdist$.rand <- rand
          pdist$.isRand <- -1L
        }

        message(paste(distribution$name, "is now decorated with FunctionImputation."))
        pdist$.updateDecorators(c(distribution$decorators, "FunctionImputation"))
        invisible(self)
      }
    }
  ),

  active = list(
    #' @field methods
    #' Returns the names of the available methods in this decorator.
    methods = function() {
      names(private)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {

      if (testDiscrete(self)) {
        data <- matrix(x, ncol = 1)
        pdf <- self$cdf(data = data) - self$cdf(data = data - 1)
      } else if (testContinuous(self)) {
        message(.distr6$message_numeric)
        pdf <- pracma::fderiv(self$cdf, x)
      }

      if (log) {
        pdf <- log(pdf)
      }

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {

      message(.distr6$message_numeric)

      if (testDiscrete(self)) {
        grid_x <- impute_genx(self, private$n_grid)
        cdf <- C_NumericCdf_Discrete(
          q = x,
          x = grid_x,
          pdf = self$pdf(grid_x),
          lower = lower.tail,
          logp = log.p
        )
      } else {
        cdf <- numeric(length(x))
        for (i in seq_along(x)) {
          cdf[i] <- integrate(self$pdf, self$properties$support$lower, x[i])$value
        }
      }

      return(cdf)
    },
    .quantile = function(p, lower.tail = TRUE, log.p = FALSE) {

      message(.distr6$message_numeric)
      data <- p

      if (testContinuous(self)) {
        x <- self$workingSupport()
        lower <- x$lower
        upper <- x$upper
        quantile <- numeric(length(data))
        for (i in seq_along(data)) {
          quantile[i] <- suppressMessages(GoFKernel::inverse(self$cdf,
            lower = lower, upper = upper
          )(data[i]))
        }
      } else {
        x <- impute_genx(self, private$n_grid)
        if (isCdf(self) == 1L) {
          quantile <- suppressMessages(C_NumericQuantile(data, x, self$cdf(x), lower.tail, log.p))
        } else {
          cdf <- suppressMessages(C_NumericCdf_Discrete(x, x, self$pdf(x),
            lower = TRUE,
            logp = FALSE
          ))
          quantile <- C_NumericQuantile(data, x, cdf, lower.tail, log.p)
        }
      }

      return(quantile)
    },
    .rand = function(n) {
      message(.distr6$message_numeric)
      if (isQuantile(self) == 1L) {
        return(self$quantile(runif(n)))
      } else if (isPdf(self) == 1L) {
        x <- impute_genx(self, private$n_grid)
        return(sample(x, n, TRUE, self$pdf(x)))
      } else {
        return(self$quantile(runif(n)))
      }
    }
  )
)

.distr6$decorators <- append(.distr6$decorators, list(FunctionImputation = FunctionImputation))
