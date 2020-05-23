#' @title Abstract Special Distribution Class
#'
#' @template class_abstract
#' @template field_package
#' @template field_packages
#' @template method_setParameterValue
#' @template param_decorators
#' @template param_support
#'
#' @export
SDistribution <- R6Class("SDistribution", inherit = Distribution,
   public = list(
      package = "This is now deprecated. Use $packages instead.",
      packages = NULL,

      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #' @param type `[set6::Set]` \cr
      #' Type of the distribution.
      #' @param symmetry `character(1)` \cr
      #' Distribution symmetry type, default "asymmetric".
      initialize = function(decorators, support, type,
                            symmetry = c("asymmetric", "symmetric")) {

         if (getR6Class(self) == "SDistribution") {
            stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Use listDistributions()
    to see the probability distributions currently implemented in distr6."))
         }

         assert_pkgload(self$packages)

         if (!is.null(decorators)) suppressMessages(decorate(self, decorators))

         private$.traits$type <- assertSet(type)

         kur <- try(self$kurtosis(excess = TRUE), silent = TRUE)
         skew <- try(self$skewness(excess = TRUE), silent = TRUE)

         private$.properties <- list(
            kurtosis = ifnerror(kur, exkurtosisType(kur), "NULL"),
            skewness = ifnerror(skew, skewnessType(skew), "NULL"),
            support = assertSet(support),
            symmetry = match.arg(symmetry)
         )

         sapply(
            c(names(Binomial$public_fields), names(Binomial$public_methods)),
            function(x) try(lockBinding(x, self), silent = TRUE)
         )
         sapply(
            c(names(Binomial$private_fields), names(Binomial$private_methods)),
            function(x) try(lockBinding(x, private), silent = TRUE)
         )

         invisible(self)
      },

      #' @description
      #' Sets the value(s) of the given parameter(s).
      setParameterValue = function(..., lst = NULL, error = "warn") {
         if (is.null(lst)) {
            lst <- list(...)
         }
         lst <- private$.getRefParams(lst)
         super$setParameterValue(lst = lst, error = error)
         invisible(self)
      }
   ),

   private = list(
      .log = TRUE,
      .isPdf =  TRUE,
      .isCdf = TRUE,
      .isQuantile =  TRUE,
      .isRand = TRUE
   )
)




