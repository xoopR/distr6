#' @title Abstract Special Distribution Class
#' @name SDistribution
#' @template class_abstract
#'
#' @seealso [listDistributions()]
#'
#' @section Methods:
#'
#' * `mode(which = "all")` - Mode of the distribution. `which` selects which
#' mode to return if multimodal
#' * `mean(...)` \deqn{E_X(X) = \sum p_X(x)*x}
#' * `variance(...)` \deqn{var_X = E[X^2] - E[X]^2}
#' * `median()` Any real-valued m where
#' \deqn{P(X \le m) \ge 0.5 and P(X \ge m) \ge 0.5}
#' * `skewness(...)`
#' \deqn{sk_X = E_X[\frac{x - \mu}{\sigma}^3]}{sk_X = E_X[((x - \mu)/\sigma)^3]}
#' * `kurtosis(excess = TRUE, ...)` \cr
#' If `excess = FALSE`
#' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4]}{k_X = E_X[((x - \mu)/\sigma)^4]}
#' If `excess = TRUE`
#' \deqn{k_X = E_X[\frac{x - \mu}{\sigma}^4] - 3}{k_X = E_X[((x - \mu)/\sigma)^4] - 3}
#' * `entropy(base = 2, ...)` \deqn{- \sum (f_X)log_{base}(f_X)}
#' * `mgf(t, ...)` \deqn{mgf_X(t) = E_X[exp(xt)]}
#' * `cf(t, ...)` \deqn{cf_X(t) = E_X[exp(xti)]}
#' * `pgf(z, ...)` \deqn{pgf_X(z) = E_X[exp(z^x)]}
#'
#' @export
NULL
SDistribution <- AbstractClass("SDistribution",
  inherit = Distribution,
  public = list(
    packages = NULL,
    initialize = function(decorators, support, type,
                          symmetry = c("asymmetric", "symmetric")) {

      # check required packages are loaded
      assert_pkgload(self$packages)

      # decorate if requested
      if (!is.null(decorators)) {
        suppressMessages(decorate(self, decorators))
      }

      private$.traits$type <- assertSet(type)
      private$.properties <- list(
        support = assertSet(support),
        symmetry = match.arg(symmetry)
      )

      if (!grepl("Empirical", getR6Class(self))) {

        args <- getR6Call()
        private$.parameters <- do.call(getParameterSet,
                                        c(list(object = self), args))
        if (length(args)) {
          linked <- private$.parameters$tag_properties$linked
          if (length(linked)) {
            vals <- unlist(lapply(linked, function(.x) {
              which <- names(private$.parameters$get_values(tags = .x, simplify = FALSE))
              if (any(names(args) %in% which)) {
                expand_list(which, args[names(args) %in% which])
              } else {
                private$.parameters$get_values(tags = .x, simplify = FALSE, transform = FALSE)
              }
            }), recursive = FALSE)

            vals <- c(args[setdiff(names(args), names(vals))], vals)
            self$setParameterValue(lst = vals)
          } else {
            self$setParameterValue(lst = args)
          }
        }
      }

      invisible(self)
    }
  ),

  private = list(
    .log = TRUE,
    .isPdf = 1L,
    .isCdf = 1L,
    .isQuantile = 1L,
    .isRand = 1L
  )
)
