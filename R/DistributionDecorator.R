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
DistributionDecorator <- DecoratorClass("DistributionDecorator",
  abstract = TRUE,
  public = list(
    print = function() {
      cat(sprintf("%s %s", as.character(self), strCollapse(private$ooplah$decorators, "[]", ",")))
    }
  )
)
