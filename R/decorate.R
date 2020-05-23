#' @title Decorate Distributions
#'
#' @description Functionality to decorate R6 Distributions (and child classes) with extra methods.
#'
#' @details Decorating is the process of adding methods to classes that are not part of the core
#' interface (Gamma et al. 1994). Use \code{listDecorators} to see which decorators are currently available. The primary
#' use-cases are to add numeric results when analytic ones are missing, to add complex modelling functions and
#' to impute missing d/p/q/r functions.
#'
#' The \code{decorators} parameter should either be a list of decorator classes or their names
#' or a single decorator class; see examples.
#'
#' @param distribution distribution to decorate
#' @param decorators vector of decorator names
#'
#'
#' @seealso \code{\link{listDecorators}} for available decorators.
#'
#' @examples
#' B <- Binomial$new()
#' decorate(B, CoreStatistics)
#'
#' E <- Exponential$new()
#' decorate(E, list(CoreStatistics, ExoticStatistics))
#'
#' E <- Exponential$new()
#' decorate(E, list(CoreStatistics, "ExoticStatistics"))
#'
#' E <- Exponential$new()
#' decorate(E, c("CoreStatistics", "ExoticStatistics"))
#' @return Returns a decorated R6 object inheriting from class SDistribution with the methods listed
#' from one of the available decorators added to the SDistribution methods.
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
decorate <- function(distribution, decorators) {

  checkmate::assertSubset(decorators, listDecorators())
  assertDistribution(distribution)

  if (length(setdiff(distribution$decorators, decorators)) == 0) {
    message(paste(dist_name, "is already decorated with", paste0(decors_names, collapse = ",")))
    return(NULL)
  } else {
    lapply(decorators, function(a_decorator) get(a_decorator)$new()$decorate(distribution))

    distribution$.__enclos_env__$private$.updateDecorators(
      c(distribution$decorators,decorators)
    )

    message(paste(dist_name, "is now decorated with", paste0(decors_names, collapse = ",")))
    return(distribution)
  }
}
