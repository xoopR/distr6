#' @title Decorate Distributions
#'
#' @description A convenient S3 function to decorate R6 distributions
#'
#' @details Decorating is the process of adding methods to classes that are not part of the core
#' interface. Use \code{listDecorators} to see wich decorators are currently available. The primary
#' use-cases are to add numeric results when analytic ones are missing, to add complex modelling functions,
#' to impute missing p/d/q/r functions.
#'
#' The \code{decorators} parameter should either be a list of decorator classes (i.e. not as strings)
#' or a single decorator class; see examples.
#'
#' @param distribution distribution to decorate
#' @param decorators list of decorators
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
#' @export
decorate <- function(distribution, decorators){
  if(!checkmate::testList(decorators))
    decorators = list(decorators)

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
            formals(methods[[i]]) = c(formals(methods[[i]]),list(self=distribution))
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
