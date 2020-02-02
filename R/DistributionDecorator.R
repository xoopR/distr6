#' @name DistributionDecorator
#'
#' @title Abstract DistributionDecorator Class
#'
#' @description The abstract parent class to decorators.
#'
#' @details Decorating is the process of adding methods to classes that are not part of the core
#' interface (Gamma et al. 1994). Use \code{listDecorators} to see which decorators are currently available. The primary
#' use-cases are to add numeric results when analytic ones are missing, to add complex modelling functions and
#' to impute missing d/p/q/r functions.
#'
#' Abstract classes cannot be implemented directly. Use the \code{decorate} function to decorate distributions.
#'
#' @seealso \code{\link{decorate}} and \code{\link{listDecorators}}
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @return Returns error. Abstract classes cannot be constructed directly.
#'
#' @export
NULL
DistributionDecorator <- R6Class("DistributionDecorator")
DistributionDecorator$set("public","initialize",function(distribution){
  if(getR6Class(self) == "DistributionDecorator")
    stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Try using
               decorate([distribution], ",getR6Class(self),")"))

  decorate(distribution, get(getR6Class(self)))
})
