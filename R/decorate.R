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
#' @param decorators list or vector of decorators. See Details.
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
#'
#' @return Returns a decorated R6 object inheriting from class SDistribution with the methods listed
#' from one of the available decorators added to the SDistribution methods.
#'
#' @references
#' Gamma, Erich, Richard Helm, Ralph Johnson, and John Vlissides. 1994. “Design Patterns: Elements
#' of Reusable Object-Oriented Software.” Addison-Wesley.
#'
#' @export
decorate <- function(distribution, decorators){
  if(!checkmate::testList(decorators)){
    if(class(decorators) == "character")
      decorators = as.list(decorators)
    else
      decorators = list(decorators)
  }


  decorators = lapply(decorators, function(x){
    if (checkmate::testCharacter(x))
      x = utils::getFromNamespace(x, "distr6")
    return(x)
    })

  dist_decors = distribution$decorators()
  decors_names = lapply(decorators, function(x) x$classname)
  decorators = decorators[!(decors_names %in% dist_decors)]

  dist_name = substitute(distribution)
  if(dist_name == ".") dist_name = distribution$short_name

  if(length(decorators) == 0){
    message(paste(dist_name,"is already decorated with", paste0(decors_names,collapse = ",")))
    return(NULL)
  }

  else{
    lapply(decorators, function(a_decorator){
      if(a_decorator$classname == "FunctionImputation"){
        if(!testUnivariate(distribution))
          stop("FunctionImputation is currently only supported for univariate distributions.")
        if(!distribution$.__enclos_env__$private$.isPdf){
          pdf = FunctionImputation$public_methods$pdf
          formals(pdf)$self = distribution
          distribution$.__enclos_env__$private$.pdf <- pdf
          distribution$.__enclos_env__$private$.isPdf <- TRUE
        }
        if(!distribution$.__enclos_env__$private$.isCdf){
          cdf = FunctionImputation$public_methods$cdf
          formals(cdf)$self = distribution
          distribution$.__enclos_env__$private$.cdf <- cdf
          distribution$.__enclos_env__$private$.isCdf <- TRUE
        }
        if(!distribution$.__enclos_env__$private$.isQuantile){
          quant = FunctionImputation$public_methods$quantile
          formals(quant)$self = distribution
          distribution$.__enclos_env__$private$.quantile <- quant
          distribution$.__enclos_env__$private$.isQuantile <- TRUE
        }
        if(!distribution$.__enclos_env__$private$.isRand){
          rand = FunctionImputation$public_methods$rand
          formals(rand)$self = distribution
          distribution$.__enclos_env__$private$.rand <- rand
          distribution$.__enclos_env__$private$.isRand <- TRUE
        }
      } else{
        methods <- c(a_decorator$public_methods, get(paste0(a_decorator$inherit))$public_methods)
        methods <- methods[!(names(methods) %in% c("initialize","clone"))]
        methods <- methods[!(names(methods) %in% ls(distribution))]

        if(length(methods) > 0){
          for(i in 1:length(methods)){
            formals(methods[[i]]) = c(formals(methods[[i]]),list(self=distribution),list(private = distribution$.__enclos_env__$private))
            assign(names(methods)[[i]],methods[[i]],envir=as.environment(distribution))
          }
        }
      }
    })

    distribution$.__enclos_env__$private$.updateDecorators(unlist(decors_names))

    message(paste(dist_name,"is now decorated with", paste0(decors_names,collapse = ",")))
    return(distribution)
  }
}
