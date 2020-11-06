#' @name DistributionWrapper
#'
#' @title Abstract DistributionWrapper Class
#'
#' @template class_wrapper
#' @template class_abstract
#' @template method_setParameterValue
#' @template method_wrappedModels
#'
#' @details Wrappers in distr6 use the composite pattern (Gamma et al. 1994), so that a wrapped
#' distribution has the same methods and fields as an unwrapped one. After wrapping, the parameters
#' of a distribution are prefixed with the distribution name to ensure uniqueness of parameter IDs.
#'
#' Use [listWrappers] function to see constructable wrappers.
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
DistributionWrapper <- R6Class("DistributionWrapper",
  inherit = Distribution, lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param distlist `(list())`\cr
    #' List of [Distribution]s.
    #' @param name `(character(1))`\cr
    #' Wrapped distribution name.
    #' @param short_name `(character(1))`\cr
    #' Wrapped distribution ID.
    #' @param description `(character())` \cr
    #' Wrapped distribution description.
    #' @param support `([set6::Set])`\cr
    #' Wrapped distribution support.
    #' @param type `([set6::Set])`\cr
    #' Wrapped distribution type.
    #' @param valueSupport `(character(1))`\cr
    #' Wrapped distribution value support.
    #' @param variateForm `(character(1))`\cr
    #' Wrapped distribution variate form.
    #' @param parameters `([ParameterSetCollection])`\cr
    #' Optional parameters to add to the internal collection, ignored if `distlist` is given.
    #' @param outerID `([ParameterSet])`\cr
    #' Parameters added by the wrapper.
    initialize = function(distlist = NULL,
                          name, short_name, description,
                          support, type, valueSupport, variateForm,
                          parameters = NULL, outerID = NULL) {

      abstract(self, "DistributionWrapper", "listWrappers()")

      self$name <- name
      self$short_name <- short_name
      self$description <- description

      private$.properties <- list(
        kurtosis = NULL,
        skewness = NULL,
        support = assertSet(support),
        symmetry = "asymmetric"
      )

      private$.traits <- list(
        valueSupport = valueSupport,
        variateForm = variateForm,
        type = assertSet(type)
      )

      paramlst <- list()
      if (!is.null(distlist)) {
        assertDistributionList(distlist)
        private$.wrappedModels <- distlist
        params <- rlapply(distlist, "parameters")
        names(params) <- names(distlist)
        paramlst <- c(paramlst, params)
      }
      if (is.null(unlist(paramlst))) paramlst <- NULL

      if (!is.null(private$.outerParameters)) {
        outerlst <- list(private$.outerParameters)
        names(outerlst) <- outerID
        paramlst <- c(paramlst, outerlst)
      }

      if (length(paramlst) != 0) {
        private$.parameters <- ParameterSetCollection$new(lst = paramlst)
      }

      if (!is.null(parameters) & is.null(distlist)) {
        assertParameterSetCollection(parameters)
        if (is.null(private$.parameters)) {
          private$.parameters <- parameters
        } else {
          private$.parameters$merge(parameters)
        }
      }

      invisible(self)
    },

    #' @description
    #' Returns model(s) wrapped by this wrapper.
    wrappedModels = function(model = NULL) {

      if (!is.null(model)) {
        if (all(model %in% names(private$.wrappedModels))) {
          if (length(model) == 1) {
            return(private$.wrappedModels[[model]])
          } else {
            return(private$.wrappedModels[model])
          }
        } else {
          private$.wrappedModels
        }
      } else {
        private$.wrappedModels
      }
    },

    #' @description
    #' Sets the value(s) of the given parameter(s).
    setParameterValue = function(..., lst = NULL, error = "warn") {
      # This case is highly unlikely to ever be seen.
      # if (length(private$.parameters) == 0) {
      #   return(NULL)
      # } else {
        if (is.null(lst)) {
          lst <- list(...)
        }
        self$parameters()$setParameterValue(lst = lst, error = error)
      # }
    }
  ),

  private = list(
    .wrappedModels = list(),
    .outerParameters = NULL,
    .isPdf = 1L,
    .isCdf = 1L,
    .isQuantile = 1L,
    .isRand = 1L,
    .log = TRUE
  )
)

#' @name wrappedModels
#' @title Gets Internally Wrapped Models
#' @description Returns either a list of all the wrapped models or the models named by parameters.
#'
#' @usage wrappedModels(object, model = NULL)
#'
#' @param object Distribution.
#' @param model character, see details.
#'
#' @return If \code{model} is NULL then returns list of models that are wrapped by the wrapper.
#' Otherwise returns model given in \code{model}.
#'
#' @export
NULL
