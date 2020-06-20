#' @title Decorate Distributions
#'
#' @description Functionality to decorate R6 Distributions (and child classes) with extra methods.
#'
#' @details Decorating is the process of adding methods to classes that are not part of the core
#' interface (Gamma et al. 1994). Use \code{listDecorators} to see which decorators are currently
#' available. The primary use-cases are to add numeric results when analytic ones are missing,
#' to add complex modelling functions and to impute missing d/p/q/r functions.
#'
#' @param distribution `([Distribution])`\cr
#' [Distribution] to decorate.
#' @param decorators `(character())`
#' Vector of [DistributionDecorator] names to decorate the [Distribution] with.
#' @param ... `ANY` \cr
#' Extra arguments passed down to specific decorators.
#'
#' @seealso [listDecorators()] for available decorators and [DistributionDecorator] for the parent
#' class.
#'
#' @examples
#' B <- Binomial$new()
#' decorate(B, "CoreStatistics")
#'
#' E <- Exponential$new()
#' decorate(E, c("CoreStatistics", "ExoticStatistics"))
#' @return Returns a [Distribution] with additional methods from the chosen
#' [DistributionDecorator].
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
decorate <- function(distribution, decorators, ...) {

  checkmate::assertSubset(decorators, listDecorators())
  assertDistribution(distribution)

  if (length(setdiff(distribution$decorators, decorators)) == 0 &
    !is.null(distribution$decorators)) {
    message(paste(
      distribution$name, "is already decorated with",
      paste0(decorators, collapse = ",")
    ))
    return(NULL)
  } else {
    if (!is.null(distribution$decorators)) {
      decorators <- setdiff(decorators, distribution$decorators)
    }

    suppressMessages(lapply(decorators, function(a_decorator) {
      get(a_decorator)$new()$decorate(distribution, ...)
    }))

    message(paste(distribution$name, "is now decorated with", paste0(decorators, collapse = ", ")))
    return(distribution)
  }
}
