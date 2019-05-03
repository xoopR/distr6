#' @title Abstract Decorator for Distribution
#'
#' @description An abstract decorator object for distributions to enhance functionality.
#' @return error
#' @name DistributionDecorator
#'
#' @param distribution distribution object
#' @param pos position of environment to assign decorated distribution
#'
#' @details This is an abstract class that cannot be directly constructed.
#'   See the concete child classes for initializable statistical decorators.
#'
#' @seealso \code{\link{CoreStatistics}} and \code{\link{ExoticStatistics}}.
NULL


#' @export
DistributionDecorator <- R6::R6Class("DistributionDecorator")

DistributionDecorator$set("public","initialize",function(distribution, pos = 1){
  if(getR6Class(self) == "DistributionDecorator")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  decorators = distribution$decorators()
  if(!is.null(decorators)){
    decorators = lapply(decorators,get)
  }
  decorators = unique(c(decorators,get(getR6Class(self))))

  if(inherits(distribution,"DistributionWrapper"))
    assign_wrappedDistribution(distribution, pos)
  else
    assign_distribution(distribution, pos)


  cat(paste(substitute(distribution),"is now decorated with",
            getR6Class(self),"\n"))
})

assign_wrappedDistribution <- function(distribution, pos){
  assign(paste0(substitute(distribution)),
         ConcreteWrapper$new(name = distribution$name(),
                             short_name = distribution$short_name(),
                             type = distribution$type(),
                             support = distribution$support(),
                             distrDomain = distribution$distrDomain(),
                             symmetric = as.logical(distribution$symmetry()),
                             pdf = distribution$.__enclos_env__$private$.pdf,
                             cdf = distribution$.__enclos_env__$private$.cdf,
                             quantile = distribution$.__enclos_env__$private$.quantile,
                             rand = distribution$.__enclos_env__$private$.rand,
                             decorators = decorators,
                             valueSupport = distribution$valueSupport(),
                             variateForm = distribution$variateForm(),
                             description = distribution$description(),
                             distlist = distribution$.__enclos_env__$private$.wrappedModels
         ), pos = as.environment(pos))
}
assign_distribution <- function(distribution, pos){
  assign(paste0(substitute(distribution)),
         Distribution$new(name = distribution$name(),
                          short_name = distribution$short_name(),
                          type = distribution$type(),
                          support = distribution$support(),
                          distrDomain = distribution$distrDomain(),
                          symmetric = as.logical(distribution$symmetry()),
                          pdf = distribution$.__enclos_env__$private$.pdf,
                          cdf = distribution$.__enclos_env__$private$.cdf,
                          quantile = distribution$.__enclos_env__$private$.quantile,
                          rand = distribution$.__enclos_env__$private$.rand,
                          parameters = distribution$.__enclos_env__$private$.parameters,
                          decorators = decorators,
                          valueSupport = distribution$valueSupport(),
                          variateForm = distribution$variateForm(),
                          description = distribution$description()
         ), pos = as.environment(pos))
}
