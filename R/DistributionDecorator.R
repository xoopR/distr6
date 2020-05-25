#' @name DistributionDecorator
#'
#' @title Abstract DistributionDecorator Class
#'
#' @template class_abstract
#' @template field_packages
#' @template method_initialize
#' @template method_decorate
#'
#' @details Decorating is the process of adding methods to classes that are not part of the core
#' interface (Gamma et al. 1994). Use [listDecorators] to see which decorators are currently
#' available. The primary use-cases are to add numeric results when analytic ones are missing,
#' to add complex modelling functions and to impute missing d/p/q/r functions.
#'
#' Use [decorate] or `$decorate` to decorate distributions.
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
DistributionDecorator <- R6Class("DistributionDecorator",
  public = list(
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      if (getR6Class(self) == "DistributionDecorator") {
        stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Try using
               decorate([distribution], ", getR6Class(self), ")"))
      }

      invisible(self)
    },

    #' @description
    #' Decorates the given distribution with the methods available in this decorator.
    decorate = function(distribution) {

      assert_pkgload(self$packages)

      methods <- setdiff(self$methods, names(distribution))

      env = as.environment(distribution)
      for (i in seq_along(methods)) {
        fun <- function() {}
        formals(fun) <- c(formals(self[[methods[[i]]]]),
                                   list(self = distribution),
                                   list(private = distribution$.__enclos_env__$private))
        body(fun) <- body(self[[methods[[i]]]])
        assign(methods[[i]], fun, envir = env)
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field methods
    #' Returns the names of the available methods in this decorator.
    methods = function() {
      setdiff(names(self),
              c(".__enclos_env__", "methods", "packages", "clone", "decorate", "initialize"))
    }
  )
)
