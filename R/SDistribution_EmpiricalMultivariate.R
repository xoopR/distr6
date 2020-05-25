#' @name EmpiricalMV
#' @template SDist
#' @templateVar ClassName EmpiricalMV
#' @templateVar DistName EmpiricalMV
#' @templateVar uses in sampling such as MCMC
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{p(x) = \sum I(x = x_i) / k}
#' @templateVar paramsupport \eqn{x_i \epsilon R, i = 1,...,k}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the support set and uniform probabilities. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar constructor samples
#' @templateVar arg1 \code{samples} \tab numeric \tab vector of observed samples. \cr
#' @templateVar constructorDets a vector of elements for the support set.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function and \code{\link{WeightedDiscrete}} for the closely related WeightedDiscrete distribution.
#'
#' @examples
#' x <- EmpiricalMV$new(stats::runif(1000) * 10)
#'
#' # d/p/q/r
#' x$pdf(1:5)
#' x$cdf(1:5) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#' @export
NULL

EmpiricalMV <- R6Class("EmpiricalMV", inherit = SDistribution, lock_objects = F,
  public = list(
    # Public fields
    name = "EmpiricalMV",
    short_name = "EmpMV",
    description = "Multivariate Empirical Probability Distribution.",

    # Public methods
    # initialize

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(data, decorators = NULL) {

      data <- data.table::as.data.table(data)
      private$.variates = ncol(data)

      private$.parameters <- ParameterSet$new(
        id = "data",
        value = list(data),
        support = UniversalSet$new(),
        settable = FALSE,
        updateFunc = NULL,
        description = "Data"
      )

      super$initialize(
        decorators = decorators,
        support = do.call(setproduct, as.Tuple(data)),
        type = Reals$new()^ncol(data)
      )
    }, # TODO

    # stats

    #' @description
    #' The arithmetic mean of a (discrete) probability distribution X is the expectation
    #' \deqn{E_X(X) = \sum p_X(x)*x}
    #' with an integration analogue for continuous distributions.
    mean = function() {
      return(mean(unlist(self$support$elements)))
    }, #TODO
    mode = function(which = "all") {
      if (which == "all") {
        return(modal(unlist(self$support$elements)))
      } else {
        return(modal(unlist(self$support$elements))[which])
      }
    }, #TODO
    variance = function() {
      return(sum((unlist(self$support$elements) - self$mean())^2) / private$.total)
    }, #TODO
    skewness = function() {
      return(sum(((unlist(self$support$elements) - self$mean()) / self$stdev())^3) / private$.total)
    }, #TODO
    kurtosis = function(excess = TRUE) {
      kurt <- sum(((unlist(self$support$elements) - self$mean()) / self$stdev())^4) / private$.total
      if (excess) {
        return(kurt - 3)
      } else {
        return(kurt)
      }
    }, #TODO
    entropy = function(base = 2) {
      p <- private$.data$N / private$.total
      return(-sum(p * log(p, base)))
    }, #TODO
    mgf = function(t) {
      if (length(t) == 1) {
        return(sum(exp(private$.data$samples * t) * (private$.data$N / private$.total)))
      } else {
        nr <- length(t)
        nc <- length(private$.data$samples)
        return(as.numeric(
          exp(matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    }, #TODO
    cf = function(t) {
      if (length(t) == 1) {
        return(sum(exp(private$.data$samples * t * 1i) * (private$.data$N / private$.total)))
      } else {
        nr <- length(t)
        nc <- length(private$.data$samples)
        return(as.complex(
          exp(matrix(private$.data$samples * 1i, nrow = nr, ncol = nc, byrow = T) *
                matrix(t, nrow = nr, ncol = nc)) %*% matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    }, #TODO
    pgf = function(z) {
      if (length(z) == 1) {
        return(sum((z^private$.data$samples) * (private$.data$N / private$.total)))
      } else {
        nr <- length(z)
        nc <- length(private$.data$samples)
        return(as.numeric(
          (matrix(z, nrow = nr, ncol = nc)^matrix(private$.data$samples, nrow = nr, ncol = nc, byrow = z)) %*%
            matrix(private$.data$N / private$.total, nrow = nc, ncol = 1)
        ))
      }
    }, #TODO

    # optional setParameterValue
    setParameterValue = function(..., lst = NULL, error = "warn") {
      message("There are no parameters to set.")
      return(NULL)
    } # TODO
  ),

  private = list(
    # dpqr
    .pdf = function(x, log = FALSE) {
      pdf <- C_EmpiricalMVPdf(x = x,
                              data = as.matrix(self$getParameterValue("data")))

      if (log) pdf <- log(pdf)

      return(pdf)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      cdf <- C_EmpiricalMVCdf(x = x,
                              data = as.matrix(self$getParameterValue("data")))

      if (!lower.tail) cdf <- 1 - cdf
      if (log.p) cdf <- log(cdf)

      return(cdf)
    },
    .rand = function(n) {
      data <- self$getParameterValue("data")
      rand <- matrix(ncol = ncol(data), nrow = n)
      for (i in seq_along(data)) {
        rand[, i] == sample(data[, i], n, TRUE)
      }
      return(rand)
    },

    # traits
    .traits = list(valueSupport = "discrete", variateForm = "multivariate"),

    .data = data.table::data.table(),
    .total = numeric(1)
  )
)

.distr6$distributions <- rbind(
  .distr6$distributions,
  data.table::data.table(
    ShortName = "EmpMV", ClassName = "EmpiricalMV",
    Type = "\u211D", ValueSupport = "discrete",
    VariateForm = "multivariate",
    Package = "-"
  )
)
