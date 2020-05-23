#' @name VectorDistribution
#' @title Vectorise Distributions
#' @description A wrapper for creating a vector of distributions.
#'
#' @details A vector distribution is intented to vectorize distributions more efficiently than storing
#' a list of distributions. To improve speed and reduce memory usage, distributions are only constructed
#' when methods (e.g. d/p/q/r) are called. Whilst it is recommended to first extract distributions
#' using `[` before querying them for results, all common methods are available in
#' `VectorDistribution` as they are wrapped in `apply`.
#'
#' @section Constructor: VectorDistribution$new(distlist = NULL, distribution = NULL, params = NULL,
#' name = NULL, short_name = NULL, description = NULL, decorators = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{distribution} \tab character \tab Distribution to wrap. \cr
#' \code{params} \tab a R object \tab Either list of parameters or matrix-type frame, see examples. \cr
#' \code{shared_params} \tab a R object \tab Either list of shared parameters or matrix-type frame, see examples. \cr
#' \code{name} \tab list \tab Optional new name for distribution. \cr
#' \code{short_name} \tab list \tab Optional new short_name for distribution. \cr
#' \code{description} \tab list \tab Optional new description for distribution. \cr
#' \code{decorators} \tab list \tab Decorators to pass to wrapped distributions on construction. \cr
#' }
#'
#' @section Constructor Details: A vector distribution can either be constructed by a list of
#' distributions passed to \code{distlist} or by passing the name of one or more distributions
#' implemented in distr6 to \code{distribution}, as well as a list or table of parameters to \code{params}.
#' The former case provides more flexibility in the ability to use wrapped distributions
#' but the latter is vastly faster for distributions of class \code{SDistribution} or custom distributions.
#' The \code{shared_params} parameter decreases memory usage and improves speed by storing any parameters
#' shared between distributions only once (instead of repeated in a list).
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}} and \code{\link{ProductDistribution}}
#'
#' @return Returns an R6 object of class VectorDistribution.
#'
#' @examples
#' # not run to save time
#' \dontrun{
#' vecDist <- VectorDistribution$new(list(Binomial$new(
#'   prob = 0.5,
#'   size = 10
#' ), Normal$new(mean = 15)))
#' vecDist$pdf(x1 = 2, x2 = 3)
#' # Equivalently
#' vecDist[1]$pdf(2)
#' vecDist[2]$pdf(3)
#' # Or to evaluate every distribution at the same point
#' vecDist$pdf(1)
#'
#' # Same wrapping for statistical functions
#' vecDist$mean()
#' c(vecDist[1]$mean(), vecDist[2]$mean())
#' vecDist$entropy()
#' c(vecDist[1]$entropy(), vecDist[2]$entropy())
#'
#' vecDist$cdf(1:5, 12:16)
#' vecDist$rand(10)
#'
#' vecBin <- VectorDistribution$new(
#'   distribution = "Binomial",
#'   params = list(
#'     list(prob = 0.1, size = 2),
#'     list(prob = 0.6, size = 4),
#'     list(prob = 0.2, size = 6)
#'   )
#' )
#' vecBin$pdf(x1 = 1, x2 = 2, x3 = 3)
#' vecBin$cdf(x1 = 1, x2 = 2, x3 = 3)
#' vecBin$rand(10)
#'
#' # Equivalently
#' vecBin <- VectorDistribution$new(
#'   distribution = "Binomial",
#'   params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))
#' )
#' vecBin$pdf(x1 = 1, x2 = 2, x3 = 3)
#' vecBin$cdf(x1 = 1, x2 = 2, x3 = 3)
#' vecBin$rand(10)
#'
#' # sharedparams is very useful for vectorized custom distributions
#' shared_params <- list(name = "A Distribution", short_name = "Dist", type = Reals$new())
#' params <- list(list(pdf = function(x) {
#'   return(1)
#' }), list(pdf = function(x) {
#'   return(2)
#' }))
#' vecdist <- VectorDistribution$new(
#'   distribution = "Distribution", params = params,
#'   shared_params = shared_params
#' )
#' vecdist$pdf(1)
#' }
#'
#' @export
NULL
VectorDistribution <- R6Class("VectorDistribution", inherit = DistributionWrapper,
  lock_objects = FALSE,
  public = list(
    initialize = function(distlist = NULL, distribution = NULL, params = NULL,
                          shared_params = NULL, name = NULL, short_name = NULL,
                          decorators = NULL) {

      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }

      if (is.null(distlist)) {
        if (is.null(distribution) | (is.null(params) & is.null(shared_params))) {
          stop("Either distlist or distribution and shared_params/params must be provided.")
        }

        distribution <- match.arg(distribution, c(listDistributions(simplify = TRUE), "Distribution"))

        if (is.null(params)) {
          params <- list()
        } else {
          if (!checkmate::testList(params)) {
            params <- apply(params, 1, as.list)
          }
        }

        if (is.null(shared_params)) {
          shared_params <- list()
        } else {
          if (!checkmate::testList(shared_params)) {
            shared_params <- as.list(shared_params)
          }
        }

        if ("short_name" %in% names(shared_params)) {
          shortname <- shared_params$short_name
          self$short_name <- if (is.null(short_name)) paste0("Vec", length(params), shortname) else short_name
        } else {
          if (distribution == "Distribution") {
            shortname <- unlist(sapply(params, function(x) x$short_name))
            self$short_name <- if (is.null(short_name)) paste0("Vec", length(params), "CustomDists") else short_name
          } else {
            shortname <- get(distribution)$public_fields$short_name
            self$short_name <- if (is.null(short_name)) paste0("Vec", length(params), shortname) else short_name
          }
        }

        private$.wrappedModels <- data.table::data.table(
          distribution = distribution,
          params = params,
          shortname = shortname
        )
        private$.wrappedModels$shortname <- makeUniqueNames(private$.wrappedModels$shortname)
        private$.sharedparams <- shared_params

        pdist <- get(distribution)
        private$.univariate <- pdist$private_fields$.traits$variateForm == "univariate"

        pdist <- pdist[["private_methods"]]
        if (!is.null(pdist[[".pdf"]])) {
          private$.pdf <- function(x1, log) {}
          body(private$.pdf) <- substitute(
            {
              fun <- function(x, log) {}
              body(fun) <- substitute(FUN)

              dpqr <- data.table()
              if (private$.univariate) {
                for (i in 1:ncol(x1)) {
                  a_dpqr <- fun(unlist(x1[, i]), log = log)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              } else {
                for (i in seq(dim(x1)[3])) {
                  mx <- x1[, , i]
                  if (class(mx)[1] == "numeric") {
                    mx = matrix(mx, nrow = 1)
                  }
                  a_dpqr <- fun(mx, log = log)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              }

              return(dpqr)
            },
            list(FUN = body(pdist[[".pdf"]]))
          )
        }

        if (!is.null(pdist[[".cdf"]])) {
          private$.cdf <- function(x1, lower.tail, log.p) {}
          body(private$.cdf) <- substitute(
            {
              fun <- function(x, lower.tail, log.p) {}
              body(fun) <- substitute(FUN)

              dpqr <- data.table()
              if (private$.univariate) {
                for (i in 1:ncol(x1)) {
                  a_dpqr <- fun(unlist(x1[, i]), lower.tail = lower.tail, log.p = log.p)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              } else {
                for (i in seq(dim(x1)[3])) {
                  a_dpqr <- fun(unlist(x1[, , i]), lower.tail = lower.tail, log.p = log.p)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              }

              return(dpqr)
            },
            list(FUN = body(pdist[[".cdf"]]))
          )
        }

        if (!is.null(pdist[[".quantile"]])) {
          private$.quantile <- function(x1, lower.tail, log.p) {}
          body(private$.quantile) <- substitute(
            {
              fun <- function(x, lower.tail, log.p) {}
              body(fun) <- substitute(FUN)

              dpqr <- data.table()
              if (private$.univariate) {
                for (i in 1:ncol(x1)) {
                  a_dpqr <- fun(unlist(x1[, i]), lower.tail = lower.tail, log.p = log.p)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              } else {
                for (i in 1:dim(x1)[3]) {
                  a_dpqr <- fun(unlist(x1[, , i]), lower.tail = lower.tail, log.p = log.p)
                  a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                  dpqr <- cbind(dpqr, a_dpqr)
                }
              }
              return(dpqr)
            },
            list(FUN = body(pdist[[".quantile"]]))
          )
        }

        if (!is.null(pdist[[".rand"]])) {
          private$.rand <- function(x1) {}
          body(private$.rand) <- substitute(
            {
              fun <- function(n) {}
              body(fun) <- substitute(FUN)

              return(fun(x1))
            },
            list(FUN = body(pdist[[".rand"]]))
          )
        }

        if (is.null(name)) {
          if (distribution == "Distribution") {
            self$name <- paste0("Vector: ", length(params), " CustomDistributions")
          } else {
            self$name <- paste0("Vector: ", length(params), " ", distribution, "s")
          }
        } else {
          self$name <- name
        }

      } else {
        private$.distlist <- TRUE
        shortname <- distribution <- c()

        vf <- distlist[[1]]$traits$variateForm
        for (i in 1:length(distlist)) {
          shortname <- c(shortname, distlist[[i]]$short_name)
          distribution <- c(distribution, distlist[[i]]$name)
          distlist[[i]] <- distlist[[i]]$clone(deep = TRUE)
          if (distlist[[i]]$traits$variateForm != vf) {
            stop("All distributions in VectorDistribution must be of same variateForm.")
          }
        }
        private$.univariate <- vf == "univariate"

        private$.wrappedModels <- data.table::data.table(
          distribution = distlist,
          params = NA,
          shortname = shortname
        )

        ndist <- length(distlist)

        private$.pdf <- function(x1, log) {
          dpqr <- data.table()
          if (private$.univariate) {
            for (i in seq(ncol(x1))) {
              a_dpqr <- self[i]$pdf(x1[, i], log = log)
              dpqr <- cbind(dpqr, a_dpqr)
            }
          } else {
            for (i in seq(dim(x1)[3])) {
              a_dpqr <- self[i]$pdf(x1[, , i], log = log)
              dpqr <- cbind(dpqr, a_dpqr)
            }
          }

          return(dpqr)
        }


        # pdf = function(x1, log) {}
        # body(pdf) = substitute({
        #   pdfs = NULL
        #   if(class(try(get("x2"), silent = T)) == "try-error"){
        #     for(i in 1:n)
        #       pdfs = c(pdfs, self[i]$pdf(get("x1")))
        #   } else {
        #     for(i in 1:n)
        #       pdfs = c(pdfs, self[i]$pdf(get(paste0("x",i))))
        #   }
        #   y = data.table::data.table(matrix(pdfs, ncol = n))
        #   colnames(y) <- unlist(private$.wrappedModels[,3])
        #   return(y)
        # },list(n = ndist))

        # cdf = function() {}
        # formals(cdf) = lst
        # body(cdf) = substitute({
        #   cdfs = NULL
        #   if(class(try(get("x2"), silent = T)) == "try-error"){
        #     for(i in 1:n)
        #       cdfs = c(cdfs, self[i]$cdf(get("x1")))
        #   } else {
        #     for(i in 1:n)
        #       cdfs = c(cdfs, self[i]$cdf(get(paste0("x",i))))
        #   }
        #   y = data.table::data.table(matrix(cdfs, ncol = n))
        #   colnames(y) <- unlist(private$.wrappedModels[,3])
        #   return(y)
        # },list(n = ndist))
        #
        # quantile = function() {}
        # formals(quantile) = lst
        # body(quantile) = substitute({
        #   quantiles = NULL
        #   if(class(try(get("x2"), silent = T)) == "try-error"){
        #     for(i in 1:n)
        #       quantiles = c(quantiles, self[i]$quantile(get("x1")))
        #   } else {
        #     for(i in 1:n)
        #       quantiles = c(quantiles, self[i]$quantile(get(paste0("x",i))))
        #   }
        #   y = data.table::data.table(matrix(quantiles, ncol = n))
        #   colnames(y) <- unlist(private$.wrappedModels[,3])
        #   return(y)
        # },list(n = ndist))
        #
        # rand = function(n) {
        #   rand <- sapply(1:nrow(private$.wrappedModels), function(x) self[x]$rand(n))
        #   if(n == 1) rand <- t(rand)
        #   rand <- data.table::as.data.table(rand)
        #   colnames(rand) <- unlist(private$.wrappedModels[,3])
        #   return(rand)
        # }
        #
        # ndist = nrow(private$.wrappedModels)

        if (length(unique(distribution)) == 1) {
          if (is.null(name)) self$name <- paste0("Vector: ", ndist, " ", distribution[[1]], "s") # FIXME
          if (is.null(short_name)) self$short_name <- paste0("Vec", ndist, private$.wrappedModels[1, 3][[1]]) # FIXME
        } else {
          if (is.null(name)) self$name <- paste("Vector:", paste0(distribution, collapse = ", ")) # FIXME
          if (is.null(short_name)) self$short_name <- paste0(private$.wrappedModels[, "shortname"][[1]], collapse = "Vec") # FIXME
        }
      }

      # self$description = description #TODO
      # private$.properties$support = setpower(Reals$new(), ndist)   # FIXME
      # private$.traits$type = setpower(Reals$new(), ndist)   # FIXME

      invisible(self)
    },

    wrappedModels = function(model = NULL) {
      if (is.null(model)) {
        if (private$.distlist) {
          return(private$.wrappedModels[, "distribution"][[1]])
        } else {
          return(apply(private$.wrappedModels, 1, function(x) do.call(get(x[[1]])$new, x[[2]])))
        }
      } else {
        model <- model[model %in% private$.wrappedModels[, "shortname"][[1]]]

        if (length(model) == 0) {
          return(self$wrappedModels())
        }

        if (private$.distlist) {
          x <- subset(private$.wrappedModels, shortname %in% model, distribution)
          if (nrow(x) == 1) {
            return(x[[1]][[1]])
          } else {
            x <- unlist(as.list(x), recursive = TRUE)
            names(x) <- model
            return(x)
          }
        } else {
          x <- subset(private$.wrappedModels, shortname %in% model)
          x <- apply(x, 1, function(y) do.call(get(y[[1]])$new, y[[2]]))
          if (length(x) == 1) {
            return(x[[1]])
          }
        }
      }
    },
    strprint = function(n = 10) {
      names <- as.character(self$modelTable$shortname)
      lng <- length(names)
      if (lng > (2 * n)) {
        names <- c(names[1:n], "...", names[(lng - n + 1):lng])
      }

      return(names)
    },

    getParameterValue = function(id) {
      if (!self$distlist) {
        if (id %in% names(self$shared_params)) {
          return(as.list(rep(self$shared_params[[id]], nrow(self$modelTable))))
        } else {
          return(lapply(self$modelTable$params, function(x) x[[id]]))
        }
      }
    },
    setParameterValue = function(...) {
      message("Vector Distribution should not be used to get/set parameters. Try to use '[' first.")
      return(NULL)
    },
    parameters = function(...){
      message("Vector Distribution should not be used to get/set parameters. Try to use '[' first.")
      return(data.table::data.table())
    },

    mean = function() {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$mean(), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    mode = function(which = "all") {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$mode(which), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    variance = function() {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$variance(), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    skewness = function() {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$skewness(), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    kurtosis = function(excess = TRUE) {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$kurtosis(excess), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    entropy = function(base = 2) {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$entropy(base), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    mgf = function(t) {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$mgf(t), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    cf = function(t) {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$cf(t), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },
    pgf = function(z) {
      ret <- matrix(sapply(1:nrow(private$.wrappedModels), function(i) {
        ifnerror(self[i]$pgf(z), error = NaN)
      }), nrow = 1)
      colnames(ret) <- unlist(private$.wrappedModels[, "shortname"])
      return(data.table::data.table(ret))
    },

    pdf = function(..., log = FALSE, data) {
      if (missing(data)) data <- as.matrix(data.table(...))

      if (private$.univariate) {
        if (ncol(data) == 1) {
          data <- matrix(rep(data, nrow(private$.wrappedModels)), nrow = nrow(data))
        }
        dpqr <- as.data.table(private$.pdf(data, log = log))
        colnames(dpqr) <- unlist(private$.wrappedModels[, 3])
        return(dpqr)
      } else {
        if (ncol(data) == 1) {
          data <- array(rep(data, nrow(private$.wrappedModels)),
                        dim = c(nrow(data), ncol(data), nrow(private$.wrappedModels))
          )
        } else if (inherits(data, "array")) {
          if (is.na(dim(data)[3])) {
            data <- array(rep(data, nrow(private$.wrappedModels)),
                          dim = c(nrow(data), ncol(data), nrow(private$.wrappedModels))
            )
          }
        }
        dpqr <- private$.pdf(data, log = log)
        colnames(dpqr) <- unlist(private$.wrappedModels[, 3])
        return(dpqr)
        #
        # nc <- prod(dim(dpqr)) / (nrow(dpqr) * dim(data)[3])
        # return(array(as.matrix(dpqr),
        #   dim = c(nrow(dpqr), nc, dim(data)[3]),
        #   dimnames = list(NULL, colnames(dpqr)[1:nc], NULL)
        # ))
      }
    },
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, data) {
      if (missing(data)) data <- as.matrix(data.table(...))

      if (private$.univariate) {
        if (ncol(data) == 1) {
          data <- matrix(rep(data, nrow(private$.wrappedModels)), nrow = nrow(data))
        }
      } else {
        if (ncol(data) == 1) {
          stop("Only one column of data but distribution is not univariate.")
        } else if (class(data) == "array") {
          if (dim(data)[3] == 1) {
            data <- array(rep(data, nrow(private$.wrappedModels)),
                          dim = c(nrow(data), ncol(data), nrow(private$.wrappedModels))
            )
          }
        }
      }

      dpqr <- as.data.table(private$.cdf(data, lower.tail = lower.tail, log.p = log.p))
      colnames(dpqr) <- unlist(private$.wrappedModels[, 3])
      return(dpqr)
    },
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, data) {
      if (missing(data)) data <- as.matrix(data.table(...))

      if (private$.univariate) {
        if (ncol(data) == 1) {
          data <- matrix(rep(data, nrow(private$.wrappedModels)), nrow = nrow(data))
        }
      } else {
        if (ncol(data) == 1) {
          stop("Only one column of data but distribution is not univariate.")
        } else if (class(data) == "array") {
          if (dim(data)[3] == 1) {
            data <- array(rep(data, nrow(private$.wrappedModels)),
                          dim = c(nrow(data), ncol(data), nrow(private$.wrappedModels))
            )
          }
        }
      }

      dpqr <- as.data.table(private$.quantile(data, lower.tail = lower.tail, log.p = log.p))
      colnames(dpqr) <- unlist(private$.wrappedModels[, 3])
      return(dpqr)
    },
    rand = function(..., data) {
      if (missing(data)) {
        data <- ...elt(1)
        if (length(data) > 1) {
          data <- length(data)
        }
      } else {
        data <- unlist(data[1, 1])
      }

      if (private$.univariate) {
        dpqr <- as.data.table(private$.rand(data))
        if (ncol(dpqr) == 1) dpqr <- transpose(dpqr)
        colnames(dpqr) <- unlist(private$.wrappedModels[, 3])
        return(dpqr)
      } else {
        dpqr <- private$.rand(data)
        dpqr <- array(unlist(dpqr), c(nrow(dpqr[[1]]), ncol(dpqr[[1]]), length(dpqr)))
        colnames(dpqr) <- paste0("V", seq(ncol(dpqr)))
        return(dpqr)
      }
    }
  ),

  active = list(
    modelTable = function() {
      private$.wrappedModels
    },
    distlist = function() {
      return(private$.distlist)
    },
    shared_params = function() {
      return(private$.sharedparams)
    },
    length = function() {
      return(nrow(self$modelTable))
    }
  ),

  private = list(
    .univariate = logical(0),
    .distlist = FALSE,
    .sharedparams = list(),
    .properties = list(),
    .traits = list(type = NA, valueSupport = "mixture", variateForm = "multivariate"
    )
  )
)

.distr6$wrappers <- append(.distr6$wrappers, list(VectorDistribution = VectorDistribution))


#' @title Extract one or more Distributions from a VectorDistribution
#' @description Once a \code{VectorDistribution} has been constructed, use \code{[}
#' to extract one or more \code{Distribution}s from inside it.
#' @param vecdist VectorDistribution from which to extract Distributions.
#' @param i indices specifying distributions to extract.
#' @export
Extract.VectorDistribution <- function(vecdist, i) {
  i <- i[i %in% (1:nrow(vecdist$modelTable))]
  if (length(i) == 0) {
    stop("Index i too large, should be less than or equal to ", nrow(vecdist$modelTable))
  }

  if (!vecdist$distlist) {
    if (length(i) == 1) {
      par <- c(vecdist$modelTable[i, 2][[1]][[1]], vecdist$shared_params)

      # if(!checkmate::testList(par))
      #   par = list(par)

      dec <- vecdist$decorators
      if (!is.null(dec)) {
        par <- c(par, list(decorators = dec))
      }

      return(do.call(get(vecdist$modelTable[i, 1][[1]])$new, par))

    } else {
      return(VectorDistribution$new(
        distribution = vecdist$modelTable[1, 1][[1]],
        params = vecdist$modelTable[i, 2][[1]]
      ))
    }
  } else {
    if (length(i) == 1) {
      dec <- vecdist$decorators
      if (!is.null(dec)) {
        dist <- vecdist$modelTable[i, 1][[1]][[1]]
        suppressMessages(decorate(dist, dec))
        return(dist)
      } else {
        return(vecdist$modelTable[i, 1][[1]][[1]])
      }
    } else {
      return(VectorDistribution$new(distlist = unlist(vecdist$modelTable[i, 1])))
    }
  }
}

#' @rdname Extract.VectorDistribution
#' @usage \method{[}{VectorDistribution}(vecdist, i)
#' @export
"[.VectorDistribution" <- function(vecdist, i) {
  Extract.VectorDistribution(vecdist, i)
}
