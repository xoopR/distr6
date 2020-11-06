#' @name VectorDistribution
#' @title Vectorise Distributions
#' @description A wrapper for creating a vector of distributions.
#' @template class_vecdist
#' @template method_setParameterValue
#' @template method_wrappedModels
#' @template method_mode
#' @template method_kurtosis
#' @template method_entropy
#' @template method_pgf
#' @template method_mgfcf
#' @template param_log
#' @template param_logp
#' @template param_simplify
#' @template param_data
#' @template param_lowertail
#' @template param_n
#' @template param_decorators
#'
#' @details A vector distribution is intented to vectorize distributions more efficiently than
#' storing a list of distributions. To improve speed and reduce memory usage, distributions are
#' only constructed when methods (e.g. d/p/q/r) are called.
#'
#' @export
VectorDistribution <- R6Class("VectorDistribution",
  inherit = DistributionWrapper,
  lock_objects = FALSE,
  lock_class = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @examples
    #' \dontrun{
    #' VectorDistribution$new(
    #'   distribution = "Binomial",
    #'   params = list(
    #'     list(prob = 0.1, size = 2),
    #'     list(prob = 0.6, size = 4),
    #'     list(prob = 0.2, size = 6)
    #'   )
    #' )
    #'
    #' VectorDistribution$new(
    #'   distribution = "Binomial",
    #'   params = data.table::data.table(prob = c(0.1, 0.6, 0.2), size = c(2, 4, 6))
    #' )
    #'
    #' # Alternatively
    #' VectorDistribution$new(
    #'   list(
    #'   Binomial$new(prob = 0.1, size = 2),
    #'   Binomial$new(prob = 0.6, size = 4),
    #'   Binomial$new(prob = 0.2, size = 6)
    #'   )
    #' )
    #' }
    initialize = function(distlist = NULL, distribution = NULL, params = NULL,
                          shared_params = NULL, name = NULL, short_name = NULL,
                          decorators = NULL, vecdist = NULL, ...) {

      #-----------------
      # Decorate wrapper
      #-----------------
      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }

      if (!is.null(vecdist)) {

        if (checkmate::testList(vecdist)) {

          dist <- as.character(unlist(vecdist[[1]]$modelTable$Distribution[[1]]))
          ids <- paste0(get(dist)$public_fields$short_name,
                       seq.int(sum(sapply(vecdist, function(.x) nrow(.x$modelTable)))))
          private$.modelTable <- as.data.table(data.frame(Distribution = dist, shortname = ids))
          private$.distlist <- FALSE
          private$.univariate <- vecdist[[1]]$.__enclos_env__$private$.univariate
          private$.pdf <- vecdist[[1]]$.__enclos_env__$private$.pdf
          private$.cdf <- vecdist[[1]]$.__enclos_env__$private$.cdf
          private$.quantile <- vecdist[[1]]$.__enclos_env__$private$.quantile
          private$.rand <- vecdist[[1]]$.__enclos_env__$private$.rand

          parameters  <- unlist(sapply(vecdist, function(.x) .x$parameters()$parameterSets))
          names(parameters) <- ids
          support <- subset(as.data.table(parameters[[1]]), select = c(id, support))
          support[, support := sapply(support, set6::setpower, power = length(ids))]
          parameters <- ParameterSetCollection$new(lst = parameters,
                            .checks = vecdist[[1]]$parameters()$.__enclos_env__$private$.checks,
                            .supports = support)

          super$initialize(
            distlist = if (vecdist[[1]]$distlist)
              unlist(lapply(vecdist, function(.x) .x$wrappedModels()), recursive = FALSE) else NULL,
            name = paste0("Vector: ", length(ids), " ", dist, "s"),
            short_name = paste0("Vec", length(ids), get(dist)$public_fields$short_name),
            description = paste0("Vector of ", length(ids), " ", dist, "s"),
            support = do.call(setproduct, lapply(vecdist, function(.x) .x$properties$support)),
            type = do.call(setproduct, lapply(vecdist, function(.x) .x$traits$type)),
            valueSupport = vecdist[[1]]$traits$valueSupport,
            variateForm = "multivariate",
            parameters = parameters
          )

          invisible(self)

        } else {

          private$.modelTable <- vecdist$modelTable
          private$.distlist <- vecdist$distlist
          private$.univariate <- vecdist$.__enclos_env__$private$.univariate
          private$.pdf <- vecdist$.__enclos_env__$private$.pdf
          private$.cdf <- vecdist$.__enclos_env__$private$.cdf
          private$.quantile <- vecdist$.__enclos_env__$private$.quantile
          private$.rand <- vecdist$.__enclos_env__$private$.rand

          parameters  <- vecdist$parameters()

          if (checkmate::testClass(vecdist, "MixtureDistribution")) {
            parameters$.__enclos_env__$private$.parametersets$mix <- NULL
          }

          super$initialize(
            distlist = distlist,
            name = vecdist$name,
            short_name = vecdist$short_name,
            description = vecdist$description,
            support = vecdist$properties$support,
            type = vecdist$traits$type,
            valueSupport = vecdist$traits$valueSupport,
            variateForm = "multivariate",
            parameters = parameters
          )

          if (is.null(name)) self$name <- gsub("Product|Mixture", "Vector", self$name)
          if (is.null(short_name)) self$short_name <- gsub("Prod|Mix", "Vec", self$short_name)
          self$description <- gsub("Product|Mixture", "Vector", self$description)

          invisible(self)
        }

      } else {

        #----------------------------------
        # distribution + params constructor
        #----------------------------------
        if (is.null(distlist)) {
          if (is.null(distribution) | (is.null(params))) {
            stop("Either distlist or distribution and params must be provided.")
          }

          distribution <- match.arg(distribution, c(listDistributions(simplify = TRUE),
                                                    listKernels(simplify = TRUE)))

          if (grepl("Empirical", distribution)) {
            stop("Empirical and EmpiricalMV not currently available for `distribution/params`
constructor, use `distlist` instead.")
          }

          # convert params to list
          if (!checkmate::testList(params)) {
            params <- apply(params, 1, as.list)
          }

          # catch for Geometric and NegativeBinomial
          if (distribution == "Geometric" & "trials" %in% names(unlist(params))) {
            stop("For Geometric distributions either `trials` must be passed to `shared_params`
or `distlist` should be used.")
          }

          if (distribution == "NegativeBinomial" &
              any(c("form", "size") %in% names(unlist(params)))) {
            stop("For NegativeBinomial distributions either `form`/`size` must be passed to
`shared_params` or `distlist` should be used.")
          }

          # convert shared_params to list
          if (is.null(shared_params)) {
            shared_params <- list()
          } else {
            if (!checkmate::testList(shared_params)) {
              shared_params <- as.list(shared_params)
            }
          }

          # create wrapper parameters by cloning distribution parameters and setting by given params
          # skip if no parameters
          pdist <- get(distribution)
          p <- try(do.call(paste0("getParameterSet.", distribution), c(params[[1]], shared_params)),
                   silent = TRUE)
          if (class(p)[[1]] != "try-error") {
            paramlst <- vector("list", length(params))
            for (i in seq_along(params)) {
              paramlst[[i]] <- p$clone(deep = TRUE)
            }

            names(paramlst) <- makeUniqueNames(rep(pdist$public_fields$short_name, length(params)))
            names(params) <- names(paramlst)
            params <- unlist(params, recursive = FALSE)
            names(params) <- gsub(".", "_", names(params), fixed = TRUE)
            if (!is.null(shared_params)) {
              if (distribution == "Geometric") {
                shared_params <- shared_params[!(names(shared_params) %in% "trials")]
              }
              if (distribution == "NegativeBinomial") {
                shared_params <- shared_params[!(names(shared_params) %in% "form")]
              }
              shared_params <- rep(list(shared_params), length(params))
              names(shared_params) <- names(paramlst)
              shared_params <- unlist(shared_params, recursive = FALSE)
              names(shared_params) <- gsub(".", "_", names(shared_params), fixed = TRUE)
              params <- c(params, shared_params)
            }

            support <- subset(as.data.table(p), select = c(id, support))
            support[, support := sapply(support, set6::setpower, power = length(paramlst))]
            parameters <- ParameterSetCollection$new(lst = paramlst, .checks = p$checks,
                                                     .supports = support)$setParameterValue(lst = params)
          } else {
            paramlst <- vector("list", length(params))
            names(paramlst) <- makeUniqueNames(rep(pdist$public_fields$short_name, length(params)))
            parameters <- ParameterSetCollection$new()
          }

          shortname <- get(distribution)$public_fields$short_name

          # modelTable is for reference and later
          # construction; coercion to table from frame due to recycling
          private$.modelTable <- as.data.table(
            data.frame(Distribution = distribution, shortname = names(paramlst))
          )

          # set univariate flag for calling d/p/q/r
          private$.univariate <- pdist$private_fields$.traits$variateForm == "univariate"
          # inheritance catch
          if (!length(private$.univariate)) {
            private$.univariate <- pdist$get_inherit()$private_fields$.trait$variateForm == "univariate"
          }
          # set valueSupport
          valueSupport <- pdist$private_fields$.traits$valueSupport
          # inheritance catch
          if (!length(valueSupport)) {
            valueSupport <- pdist$get_inherit()$private_fields$.trait$valueSupport
          }

          # set d/p/q/r if non-NULL
          pdist_pri <- pdist[["private_methods"]]
          if (!is.null(pdist_pri[[".pdf"]])) {
            private$.pdf <- function(x1, log) {}
            body(private$.pdf) <- substitute(
              {
                fun <- function(x, log) {}
                body(fun) <- substitute(FUN)

                dpqr <- data.table()
                if (private$.univariate) {
                  if (ncol(x1) == 1) {
                    dpqr <- fun(unlist(x1), log = log)
                  } else if (nrow(x1) == 1) {
                    dpqr <- fun(x1, log = log)
                    if (nrow(dpqr) > 1) {
                      dpqr <- diag(as.matrix(dpqr))
                    }
                  } else {
                    for (i in seq_len(ncol(x1))) {
                      a_dpqr <- fun(unlist(x1[, i]), log = log)
                      a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                      dpqr <- cbind(dpqr, a_dpqr)
                    }
                  }
                } else {
                  if (length(dim(x1)) == 2) {
                    dpqr <- data.table(matrix(fun(x1, log = log), nrow = nrow(x1)))
                  } else {
                    for (i in seq_len(dim(x1)[3])) {
                      mx <- x1[, , i]
                      if (class(mx)[1] == "numeric") {
                        mx <- matrix(mx, nrow = 1)
                      }
                      a_dpqr <- fun(mx, log = log)
                      a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                      dpqr <- cbind(dpqr, a_dpqr)
                    }
                  }
                }

                return(dpqr)
              },
              list(FUN = body(pdist_pri[[".pdf"]]))
            )
          }
          if (!is.null(pdist_pri[[".cdf"]])) {
            private$.cdf <- function(x1, lower.tail, log.p) {}
            body(private$.cdf) <- substitute(
              {
                fun <- function(x, lower.tail, log.p) {}
                body(fun) <- substitute(FUN)

                dpqr <- data.table()
                if (private$.univariate) {
                  if (ncol(x1) == 1) {
                    dpqr <- fun(unlist(x1), lower.tail = lower.tail, log.p = log.p)
                  } else if (nrow(x1) == 1) {
                    dpqr <- fun(x1, lower.tail = lower.tail, log.p = log.p)
                    if (nrow(dpqr) > 1) {
                      dpqr <- diag(as.matrix(dpqr))
                    }
                  } else {
                    for (i in seq(ncol(x1))) {
                      a_dpqr <- fun(unlist(x1[, i]), lower.tail = lower.tail, log.p = log.p)
                      a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                      dpqr <- cbind(dpqr, a_dpqr)
                    }
                  }
                }
                # TODO - This will be uncommented once EmpiricalMV can be used here
                # else {
                # for (i in seq(dim(x1)[3])) {
                #   a_dpqr <- fun(unlist(x1[, , i]), lower.tail = lower.tail, log.p = log.p)
                #   a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                #   dpqr <- cbind(dpqr, a_dpqr)
                # }
                #}

                return(dpqr)
              },
              list(FUN = body(pdist_pri[[".cdf"]]))
            )
          }
          if (!is.null(pdist_pri[[".quantile"]])) {
            private$.quantile <- function(x1, lower.tail, log.p) {}
            body(private$.quantile) <- substitute(
              {
                fun <- function(p, lower.tail, log.p) {}
                body(fun) <- substitute(FUN)

                dpqr <- data.table()
                if (ncol(x1) == 1) {
                  dpqr <- fun(unlist(x1), lower.tail = lower.tail, log.p = log.p)
                } else if (nrow(x1) == 1) {
                  dpqr <- fun(x1, lower.tail = lower.tail, log.p = log.p)
                  if (nrow(dpqr) > 1) {
                    dpqr <- diag(as.matrix(dpqr))
                  }
                } else {
                  for (i in seq_len(ncol(x1))) {
                    a_dpqr <- fun(unlist(x1[, i]), lower.tail = lower.tail, log.p = log.p)
                    a_dpqr <- if (class(a_dpqr)[1] == "numeric") a_dpqr[i] else a_dpqr[, i]
                    dpqr <- cbind(dpqr, a_dpqr)
                  }
                }

                return(dpqr)
              },
              list(FUN = body(pdist_pri[[".quantile"]]))
            )
          }
          if (!is.null(pdist_pri[[".rand"]])) {
            private$.rand <- function(x1) {}
            body(private$.rand) <- substitute(
              {
                fun <- function(n) {}
                body(fun) <- substitute(FUN)

                return(fun(x1))
              },
              list(FUN = body(pdist_pri[[".rand"]]))
            )
          }

          #----------------------------------
          # ditlist constructor
          #----------------------------------
        } else {
          # set flag to TRUE
          private$.distlist <- TRUE
          shortname <- distribution <- c()

          # get all parameters in a list
          # assert all variateForm the same
          # collect valueSupport, short_name, name
          vf <- distlist[[1]]$traits$variateForm
          paramlst <- vector("list", length(distlist))
          vs <- distlist[[1]]$traits$valueSupport
          for (i in seq_along(distlist)) {
            stopifnot(distlist[[i]]$traits$variateForm == vf)
            shortname <- c(shortname, distlist[[i]]$short_name)
            distribution <- c(distribution, distlist[[i]]$name)
            if (!is.null(distlist[[i]]$parameters()))
              paramlst[[i]] <- distlist[[i]]$parameters()
            vs <- c(vs, distlist[[i]]$traits$valueSupport)
          }
          valueSupport <- if (length(unique(vs)) == 1) vs[[1]] else "mixture"
          shortname <- makeUniqueNames(shortname)

          if (is.null(unlist(paramlst))) {
            parameters <- ParameterSetCollection$new()
            paramlst <- NULL
          } else {
            names(paramlst) <- shortname
            parameters <- ParameterSetCollection$new(lst = paramlst)
          }

          names(distlist) <- shortname

          private$.univariate <- vf == "univariate"

          # modelTable is for reference and later
          # construction; coercion to table from frame due to recycling
          private$.modelTable <- as.data.table(
            data.frame(Distribution = distribution, shortname = shortname)
          )

          # set dpqr
          private$.pdf <- function(x, log = FALSE) {
            dpqr <- data.table()
            if (private$.univariate) {
              for (i in seq_len(ncol(x))) {
                a_dpqr <- self[i]$pdf(x[, i], log = log)
                dpqr <- cbind(dpqr, a_dpqr)
              }
            } else {
              for (i in seq_len(dim(x)[[3]])) {
                a_dpqr <- self[i]$pdf(data = matrix(x[, , i], nrow = nrow(x), ncol = ncol(x)),
                                      log = log)
                dpqr <- cbind(dpqr, a_dpqr)
              }
            }

            return(dpqr)
          }
          private$.cdf <- function(x, lower.tail = TRUE, log.p = FALSE) {
            dpqr <- data.table()
            if (private$.univariate) {
              for (i in seq(ncol(x))) {
                a_dpqr <- self[i]$cdf(x[, i], lower.tail = lower.tail, log.p = log.p)
                dpqr <- cbind(dpqr, a_dpqr)
              }
            } else {
              for (i in seq(dim(x)[3])) {
                a_dpqr <- self[i]$cdf(data = matrix(x[, , i], nrow = nrow(x), ncol = ncol(x)),
                                      lower.tail = lower.tail, log.p = log.p)
                dpqr <- cbind(dpqr, a_dpqr)
              }
            }

            return(dpqr)
          }
          private$.quantile <- function(x, lower.tail = TRUE, log.p = FALSE) {
            dpqr <- data.table()
            for (i in seq(ncol(x))) {
              a_dpqr <- self[i]$quantile(x[, i], lower.tail = lower.tail, log.p = log.p)
              dpqr <- cbind(dpqr, a_dpqr)
            }

            return(dpqr)
          }
          private$.rand <- function(n) {
            if (private$.univariate) {
              if (n == 1) {
                return(matrix(sapply(self$wrappedModels(), function(x) x$rand(n)), nrow = 1))
              } else {
                return(sapply(self$wrappedModels(), function(x) x$rand(n)))
              }
            } else {
              return(lapply(self$wrappedModels(), function(x) x$rand(n)))
            }

          }
        }

        # define number of distributions from modelTable
        ndist <- nrow(private$.modelTable)

        # create name, short_name, description, type, support
        dst <- unique(self$modelTable$Distribution)
        if (length(dst) == 1 & dst[[1]] %in% c(listDistributions(simplify = TRUE),
                                               listKernels(simplify = TRUE))) {
          distribution <- get(as.character(unlist(self$modelTable[1, 1])))
          if (is.null(name)) {
            name <- paste0(
              "Vector: ", ndist, " ",
              distribution$public_fields$name, "s"
            )
          }
          if (is.null(short_name)) {
            short_name <- paste0(
              "Vec", ndist,
              distribution$public_fields$short_name
            )
          }
          description <- paste0("Vector of ", ndist, " ", distribution$public_fields$name, "s")
          type <- distribution$new()$traits$type^ndist
          # FIXME - support defined as same as type
          support <- type
        } else {
          type <- do.call(setproduct, lapply(distlist, function(x) x$traits$type))
          # FIXME - support defined as same as type
          support <- type

          # name depends on length of distributions, anything over 3 shortened
          if (ndist > 3) {
            if (is.null(name)) name <- paste("Vector:", ndist, "Distributions")
            if (is.null(short_name)) short_name <- paste0("Vec", ndist, "Dists")
            description <- paste0("Vector of ", ndist, " distributions.")
          } else {
            if (is.null(name)) name <- paste("Vector:", paste0(distribution, collapse = ", "))
            if (is.null(short_name)) short_name <- paste0(shortname, collapse = "Vec")
            description <- paste0("Vector of: ", paste0(shortname, collapse = ", "))
          }
        }

        super$initialize(
          distlist = distlist,
          name = name,
          short_name = short_name,
          description = description,
          support = support,
          type = type,
          valueSupport = valueSupport,
          variateForm = "multivariate",
          parameters = parameters,
          ...
        )

      }
    },

    #' @description
    #' Returns model(s) wrapped by this wrapper.
    wrappedModels = function(model = NULL) {
      if (is.null(model)) {
        if (private$.distlist) {
          distlist <- private$.wrappedModels
        } else {
          distlist <- lapply(private$.modelTable$shortname, function(x) {
            do.call(
              get(as.character(unlist(private$.modelTable$Distribution[[1]])))$new,
              self$parameters()[paste0(x, "_")]$values()
            )
          })
        }
      } else {
        models <- subset(private$.modelTable, shortname == model)$shortname

        if (length(models) == 0) {
          stop(sprintf("No distribution called %s.", model))
        }

        if (private$.distlist) {
          distlist <- private$.wrappedModels[models]
        } else {
          distlist <- lapply(models, function(x) {
            do.call(
              get(as.character(unlist(private$.modelTable$Distribution[[1]])))$new,
              self$parameters()[paste0(x, "_")]$values()
            )
          })
        }
      }

      if (length(distlist) == 1) {
        return(distlist[[1]])
      } else {
        names(distlist) <- as.character(unlist(private$.modelTable$shortname))
        return(distlist)
      }
    },

    #' @description
    #' Printable string representation of the `VectorDistribution`. Primarily used internally.
    #' @param n `(integer(1))`\cr
    #' Number of distributions to include when printing.
    strprint = function(n = 10) {
      names <- as.character(unlist(self$modelTable$shortname))
      lng <- length(names)
      if (lng > (2 * n)) {
        names <- c(names[1:n], "...", names[(lng - n + 1):lng])
      }

      return(names)
    },

    #' @description
    #' Returns named vector of means from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    mean = function(...) {
      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$mean(...), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$mean
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$mean
        }
        if (is.null(f)) {
          stop("Not implemented for this distribution.")
        }
        formals(f) <- c(list(self = self), alist(... = ))
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        ret <- data.table(t(ret))
        colnames(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of modes from each wrapped [Distribution].
    mode = function(which = "all") {
      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$mode(which), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$mode
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$mode
        }
        if (is.null(f)) {
          stop("Not implemented for this distribution.")
        }
        formals(f) <- list(self = self, which = which)
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        # hacky catch for MVN
        if (as.character(unlist(self$modelTable$Distribution[[1]])) != "MultivariateNormal") {
          ret <- t(ret)
        }

        ret <- data.table(ret)
        colnames(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of medians from each wrapped [Distribution].
    median = function() {
      self$quantile(0.5)
    },

    #' @description
    #' Returns named vector of variances from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    variance = function(...) {
      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$variance(...), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$variance
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$variance
        }
        if (is.null(f)) {
          stop("Not implemented for this distribution.")
        }
        formals(f) <- c(list(self = self), alist(... = ))
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        # catch for covariance matrices
        dimnames(ret)[3] <- as.list(private$.modelTable[, "shortname"])
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of skewness from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    skewness = function(...) {
      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$skewness(...), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$skewness
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$skewness
        }
        if (is.null(f)) {
          stop("Not implemented for this distribution.")
        }
        formals(f) <- c(list(self = self), alist(... = ))
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))

      return(ret)
    },

    #' @description
    #' Returns named vector of kurtosis from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    kurtosis = function(excess = TRUE, ...) {

      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$kurtosis(excess, ...), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$kurtosis
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$kurtosis
        }
        if (is.null(f)) {
          stop("Not implemented for this distribution.")
        }
        formals(f) <- c(list(self = self, excess = excess), alist(... = ))
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))

      return(ret)
    },

    #' @description
    #' Returns named vector of entropy from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    entropy = function(base = 2, ...) {
      if (self$distlist) {
        ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
          ifnerror(self[i]$entropy(base, ...), error = NaN)
        })
      } else {
        f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$entropy
        if (is.null(f)) {
          f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$get_inherit()$
            public_methods$entropy
        }
        formals(f) <- c(list(self = self, base = base), alist(... = ))
        ret <- f()
        if (length(ret) == 1) {
          ret <- rep(ret, nrow(self$modelTable))
        }
      }

      names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))

      return(ret)
    },

    #' @description
    #' Returns named vector of mgf from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    mgf = function(t, ...) {
      if (!self$distlist) {
        warning("mgf not currently efficiently vectorised, may be slow.")
      }

      ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
        ifnerror(self[i]$mgf(t, ...), error = NaN)
      })

      # FIXME - VECTORISE PROPERLY
      # } else {
      #   f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$mgf
      #   formals(f) = list(self = self, t = t)
      #   ret <- f()
      # }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        ret <- data.table(ret)
        colnames(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of cf from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    cf = function(t, ...) {
      if (!self$distlist) {
        warning("cf not currently efficiently vectorised, may be slow.")
      }

      ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
        ifnerror(self[i]$cf(t, ...), error = NaN)
      })

      # FIXME - VECTORISE PROPERLY
      # } else {
      #   f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$cf
      #   formals(f) = list(self = self, t = t)
      #   ret <- f()
      # }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        ret <- data.table(ret)
        colnames(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of pgf from each wrapped [Distribution].
    #' @param ... Passed to [CoreStatistics]`$genExp` if numeric.
    pgf = function(z, ...) {
      if (!self$distlist) {
        warning("pgf not currently efficiently vectorised, may be slow.")
      }

      ret <- sapply(seq(nrow(private$.modelTable)), function(i) {
        ifnerror(self[i]$pgf(z, ...), error = NaN)
      })

      # FIXME - VECTORISE PROPERLY
      # } else {
      #   f <- get(as.character(unlist(self$modelTable$Distribution[[1]])))$public_methods$pgf
      #   formals(f) = list(self = self, z = z)
      #   ret <- f()
      # }

      if (is.null(dim(ret))) {
        names(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      } else {
        ret <- data.table(ret)
        colnames(ret) <- as.character(unlist(private$.modelTable[, "shortname"]))
      }

      return(ret)
    },

    #' @description
    #' Returns named vector of pdfs from each wrapped [Distribution].
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    #' @examples
    #' vd <- VectorDistribution$new(
    #'  distribution = "Binomial",
    #'  params = data.frame(size = 9:10, prob = c(0.5,0.6)))
    #'
    #' vd$pdf(2)
    #' # Equivalently
    #' vd$pdf(2, 2)
    #'
    #' vd$pdf(1:2, 3:4)
    #' # or as a matrix
    #' vd$pdf(data = matrix(1:4, nrow = 2))
    #'
    #' # when wrapping multivariate distributions, arrays are required
    #' vd <- VectorDistribution$new(
    #'  distribution = "Multinomial",
    #'  params = list(
    #'  list(size = 5, probs = c(0.1, 0.9)),
    #'  list(size = 8, probs = c(0.3, 0.7))
    #'  )
    #'  )
    #'
    #' # evaluates Multinom1 and Multinom2 at (1, 4)
    #' vd$pdf(1, 4)
    #'
    #' # evaluates Multinom1 at (1, 4) and Multinom2 at (5, 3)
    #' vd$pdf(data = array(c(1,4,5,3), dim = c(1,2,2)))
    #'
    #' # and the same across many samples
    #' vd$pdf(data = array(c(1,2,4,3,5,1,3,7), dim = c(2,2,2)))
    pdf = function(..., log = FALSE, simplify = TRUE, data = NULL) {
      if (is.null(data)) {
        data <- as.matrix(data.table(...))
      } else if (length(dim(data)) == 2) {
        data <- as.matrix(data)
      }

      if (ncol(data) != nrow(self$modelTable) & ncol(data) > 1 & private$.univariate) {
        stopf("Expected data with %s or 1 columns, received %s.", nrow(self$modelTable), ncol(data))
      }

      if (private$.univariate) {
        if (private$.distlist & ncol(data) == 1) {
          data <- matrix(rep(data, nrow(private$.modelTable)), nrow = nrow(data),
                         ncol = nrow(private$.modelTable))
        }
        dpqr <- private$.pdf(data, log = log)
        if (class(dpqr)[1] == "numeric") {
          dpqr <- matrix(dpqr, ncol = nrow(private$.modelTable))
        }
        dpqr <- as.data.table(dpqr)
        colnames(dpqr) <- as.character(unlist(private$.modelTable[, 2]))
        return(dpqr)
      } else {
        if (ncol(data) == 1) {
          stop("Distribution is multivariate but values have only been passed to one argument.")
        }
        if ((inherits(data, "array") | inherits(data, "matrix")) & private$.distlist) {
          if (is.na(dim(data)[3])) {
            data <- array(rep(data, nrow(private$.modelTable)),
              dim = c(nrow(data), ncol(data), nrow(private$.modelTable))
            )
          }
        }
        dpqr <- private$.pdf(data, log = log)
        colnames(dpqr) <- as.character(unlist(private$.modelTable[, 2]))
        return(dpqr)
      }
    },

    #' @description
    #' Returns named vector of cdfs from each wrapped [Distribution].
    #' Same usage as `$pdf.`
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    cdf = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {
      if (is.null(data)) {
        data <- as.matrix(data.table(...))
      }

      if (ncol(data) != nrow(self$modelTable) & ncol(data) > 1 & private$.univariate) {
        stopf("Expected data with %s or 1 columns, received %s.", nrow(self$modelTable), ncol(data))
      }

      if (private$.univariate) {
        if (ncol(data) == 1 & private$.distlist) {
          data <- matrix(rep(data, nrow(private$.modelTable)), nrow = nrow(data))
        }
      } else {
        if (ncol(data) == 1) {
          stop("Distribution is multivariate but values have only been passed to one argument.")
        } else if (inherits(data, "array") | inherits(data, "matrix")) {
          if (is.na(dim(data)[3]) & private$.distlist) {
            data <- array(rep(data, nrow(private$.modelTable)),
                          dim = c(nrow(data), ncol(data), nrow(private$.modelTable))
            )
          }
        }
      }

      dpqr <- private$.cdf(data, lower.tail = lower.tail, log.p = log.p)
      if (class(dpqr)[1] == "numeric") {
        dpqr <- matrix(dpqr, ncol = nrow(private$.modelTable))
      }
      dpqr <- as.data.table(dpqr)
      colnames(dpqr) <- as.character(unlist(private$.modelTable[, 2]))
      return(dpqr)
    },

    #' @description
    #' Returns named vector of quantiles from each wrapped [Distribution].
    #' Same usage as `$cdf.`
    #' @param ... `(numeric())` \cr
    #' Points to evaluate the function at Arguments do not need
    #' to be named. The length of each argument corresponds to the number of points to evaluate,
    #' the number of arguments corresponds to the number of variables in the distribution.
    #' See examples.
    quantile = function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {
      if (is.null(data)) {
        data <- as.matrix(data.table(...))
      }

      if (ncol(data) != nrow(self$modelTable) & ncol(data) > 1 & private$.univariate) {
        stopf("Expected data with %s or 1 columns, received %s.", nrow(self$modelTable), ncol(data))
      }

      if (private$.univariate) {
        if (ncol(data) == 1 & private$.distlist) {
          data <- matrix(rep(data, nrow(private$.modelTable)), nrow = nrow(data))
        }
      } else {
        stop("Quantile not possible for non-univariate distributions.")
      }

      dpqr <- private$.quantile(data, lower.tail = lower.tail, log.p = log.p)
      if (class(dpqr)[1] == "numeric") {
        dpqr <- matrix(dpqr, ncol = nrow(private$.modelTable))
      }
      dpqr <- as.data.table(dpqr)
      colnames(dpqr) <- as.character(unlist(private$.modelTable[, 2]))
      return(dpqr)
    },

    #' @description
    #' Returns [data.table::data.table] of draws from each wrapped [Distribution].
    rand = function(n, simplify = TRUE) {
      if (length(n) > 1) {
        n <- length(n)
      }

      data <- n

      if (private$.univariate) {
        dpqr <- as.data.table(private$.rand(data))
        colnames(dpqr) <- as.character(unlist(private$.modelTable[, 2]))
        return(dpqr)
      } else {
        dpqr <- private$.rand(data)
        dpqr <- array(unlist(dpqr), c(nrow(dpqr[[1]]), ncol(dpqr[[1]]), length(dpqr)))
        dimnames(dpqr) <- list(NULL, paste0("V", seq(ncol(dpqr))),
                               as.character(unlist(private$.modelTable$shortname)))
        return(dpqr)
      }
    }
  ),

  active = list(
    #' @field modelTable
    #' Returns reference table of wrapped [Distribution]s.
    modelTable = function() {
      private$.modelTable
    },
    #' @field distlist
    #' Returns list of constructed wrapped [Distribution]s.
    distlist = function() {
      return(private$.distlist)
    }
  ),

  private = list(
    .univariate = logical(0),
    .distlist = FALSE,
    .sharedparams = list(),
    .properties = list(),
    .traits = list(type = NA, valueSupport = "mixture", variateForm = "multivariate"),
    .trials = logical(0)
  )
)

.distr6$wrappers <- append(.distr6$wrappers, list(VectorDistribution = VectorDistribution))


#' @title Extract one or more Distributions from a VectorDistribution
#' @description Once a \code{VectorDistribution} has been constructed, use \code{[}
#' to extract one or more \code{Distribution}s from inside it.
#' @param vecdist VectorDistribution from which to extract Distributions.
#' @param i indices specifying distributions to extract.
#' @usage \method{[}{VectorDistribution}(vecdist, i)
#' @export
"[.VectorDistribution" <- function(vecdist, i) {
  i <- i[i %in% (seq_len(nrow(vecdist$modelTable)))]
  if (length(i) == 0) {
    stop("Index i too large, should be less than or equal to ", nrow(vecdist$modelTable))
  }

  decorators <- vecdist$decorators

  if (!vecdist$distlist) {
    distribution <- as.character(unlist(vecdist$modelTable[1, 1]))
    if (length(i) == 1) {
      id <- as.character(unlist(vecdist$modelTable[i, 2]))
      pars <- vecdist$parameters()[paste0(id, "_")]$values(FALSE)
      if (!is.null(decorators)) {
        pars <- c(pars, list(decorators = decorators))
      }
      return(do.call(get(distribution)$new, pars))
    } else {
      id <- as.character(unlist(vecdist$modelTable[i, 2]))
      pars <- vecdist$parameters()$values()
      pars <- pars[names(pars) %in% id]

      return(VectorDistribution$new(
        distribution = distribution, params = pars,
        decorators = decorators
      ))
    }
  } else {
    if (length(i) == 1) {
      dist <- vecdist$wrappedModels()[[i]]
      if (!is.null(decorators)) {
        suppressMessages(decorate(dist, decorators))
      }
      return(dist)
    } else {
      return(VectorDistribution$new(
        distlist = vecdist$wrappedModels()[i],
        decorators = decorators
      ))
    }
  }
}

#' @title Coercion to Vector Distribution
#' @description Helper functions to quickly convert compatible objects to
#' a [VectorDistribution].
#' @param object [MixtureDistribution] or [ProductDistribution]
#' @export
as.VectorDistribution <- function(object) {
  if (checkmate::testClass(object, "VectorDistribution")) {
    return(VectorDistribution$new(vecdist = object))
  } else {
    stop("Object must inherit from VectorDistribution.")
  }
}
