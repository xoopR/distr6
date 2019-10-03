#' @name DistributionWrapper
#'
#' @title Abstract DistributionWrapper Class
#'
#' @description The abstract parent class to wrappers.
#'
#' @details Wrappers in distr6 use the composite pattern (Gamma et al. 1994), so that a wrapped distribution
#' has the same methods and fields as an unwrapped one. After wrapping, the parameters of a distribution
#' are prefixed with the distribution name to ensure uniqueness of parameter IDs.
#'
#' Abstract classes cannot be implemented directly. Use the \code{listWrappers} function to see constructable wrappers.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @inheritSection SDistribution Public Variables
#'
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
#'   \code{wrappedModels(model = NULL)} \tab \code{\link{wrappedModels}} \cr
#'   \code{decorators()} \tab \code{\link{decorators}} \cr
#'   \code{traits()} \tab \code{\link{traits}} \cr
#'   \code{valueSupport()} \tab \code{\link{valueSupport}} \cr
#'   \code{variateForm()} \tab \code{\link{variateForm}} \cr
#'   \code{type()} \tab \code{\link{type}} \cr
#'   \code{properties()} \tab \code{\link{properties}} \cr
#'   \code{support()} \tab \code{\link{support}} \cr
#'   \code{symmetry()} \tab \code{\link{symmetry}} \cr
#'   \code{sup()}  \tab \code{\link{sup}} \cr
#'   \code{inf()} \tab \code{\link{inf}} \cr
#'   \code{dmax()}  \tab \code{\link{dmax}} \cr
#'   \code{dmin()} \tab \code{\link{dmin}} \cr
#'   \code{skewnessType()} \tab \code{\link{skewnessType}} \cr
#'   \code{kurtosisType()} \tab \code{\link{kurtosisType}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{d/p/q/r Methods} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n, simplify = TRUE)} \tab \code{\link{rand}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Statistical Methods} \tab \strong{Link} \cr
#'   \code{prec()} \tab \code{\link{prec}} \cr
#'   \code{stdev()} \tab \code{\link{stdev}}\cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   \code{cor()} \tab \code{\link{cor}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Parameter Methods} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(..., lst = NULL, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInType}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{strprint(n = 2)} \tab \code{\link{strprint}} \cr
#'   \code{print(n = 2)} \tab \code{\link[base]{print}} \cr
#'   \code{summary(full = T)} \tab \code{\link{summary.Distribution}} \cr
#'   }
#' @section Active Bindings:
#'  \tabular{ll}{
#'   \strong{Active Binding} \tab \strong{Link} \cr
#'   \code{isPdf} \tab \code{\link{isPdf}} \cr
#'   \code{isCdf} \tab \code{\link{isCdf}} \cr
#'   \code{isQuantile} \tab \code{\link{isQuantile}} \cr
#'   \code{isRand} \tab \code{\link{isRand}} \cr
#'   }
#'
#'
#' @return Returns error. Abstract classes cannot be constructed directly.
#'
#'@export
NULL
DistributionWrapper <- R6::R6Class("DistributionWrapper", inherit = Distribution, lock_objects = FALSE)
DistributionWrapper$set("public","initialize",function(distlist,...){
  if(getR6Class(self) == "DistributionWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertDistributionList(distlist)

  #lapply(distlist, function(x) x$parameters()$update())
  private$.wrappedModels <- distlist

  params <- data.table::rbindlist(lapply(distlist, function(x){
    params = x[["parameters"]]()$as.data.table()
    params[,1] = paste(x[["short_name"]],unlist(params[,1]),sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  if(!is.null(private$.outerParameters))
    params <- rbind(params, private$.outerParameters$as.data.table())
  params <- as.ParameterSet(params)

  super$initialize(parameters = params, ...)
})

#' @name wrappedModels
#' @title Gets Internally Wrapped Models
#' @description Returns either a list of all the wrapped models or the models named by parameters.
#'
#' @usage wrappedModels(object, model = NULL)
#' @section R6 Usage: $wrappedModels(model = NULL)
#'
#' @param object Distribution.
#' @param model character, see details.
#'
#' @details Accessor for internally wrapped models. If the \code{model} parameter is matched by a single named
#' wrapped model, this model is returned. If a vector is supplied to \code{model} parameter then a list
#' of internal models is returned if matched, otherwise a list of all internal models is returned. If
#' \code{model} is NULL (default) then a list of all internal models are returned.
#'
#' @return If \code{model} is NULL then returns list of models that are wrapped by the wrapper. Otherwise
#' returns model given in \code{model}.
#'
#' @seealso \code{\link{DistributionWrapper}}
#'
#' @export
NULL
DistributionWrapper$set("public", "wrappedModels", function(model=NULL){

  if(!is.null(model)){
    if(all(model %in% names(private$.wrappedModels))){
      if(length(model)==1)
        return(private$.wrappedModels[[model]])
      else
        return(private$.wrappedModels[model])
    } else
      private$.wrappedModels
  } else
    private$.wrappedModels
})
DistributionWrapper$set("private", ".wrappedModels", list())
DistributionWrapper$set("private", ".outerParameters", NULL)
DistributionWrapper$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)

  for(i in 1:length(lst)){
    if(grepl("_",names(lst)[[i]],fixed = T)){
      id = names(lst)[[i]]
      underscore = gregexpr("_",id,fixed=T)[[1]][1]
      model = substr(id,1,underscore-1)
      parameter = substr(id,underscore+1,1000)

      newlst = list(lst[[i]])
      names(newlst) = parameter
      self$wrappedModels(model)$setParameterValue(lst = newlst, error = error)
    } else{
      newlst = list(lst[[i]])
      names(newlst) = names(lst)[[i]]
      private$.outerParameters$setParameterValue(lst = newlst)
    }
  }

  params <- data.table::rbindlist(lapply(self$wrappedModels(), function(x){
    params = x[["parameters"]]()$as.data.table()
    params[,1] = paste(x[["short_name"]],unlist(params[,1]),sep="_")
    return(params)
  }))
  if(!is.null(private$.outerParameters))
    params <- rbind(params, private$.outerParameters$as.data.table())
  row.names(params) <- NULL
  private$.parameters <- as.ParameterSet(params)

  invisible(self)
})

