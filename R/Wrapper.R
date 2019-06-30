#' @name DistributionWrapper
#' @title Abstract Wrapper for Distributions
#' @description An R6 abstract wrapper class with methods implemented for child classes.
#'
#' @details This wrapper is an abstract class and cannot be implemented directly.
#' See \code{\link{listWrappers}} for a list of wrappers that can be constructed. After wrapping multiple models,
#' parameter IDs are altered by prefixing the ID with "model_". For example wrapping Model1 with a parameter
#' 'param1' results in 'Model1_param1'. Call \code{parameters} to find the parameter IDs.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @section Public Variables:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab Name of distribution. \cr
#'   \code{short_name} \tab Id of distribution. \cr
#'   \code{description} \tab Brief description of distribution. \cr
#'   \code{traits} \tab List: type, valueSupport, variateForm. \cr
#'   \code{package} \tab The package p/d/q/r are implemented in.
#'  }
#'
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
#'   \code{wrappedModels(model = NULL)} \tab \code{\link{wrappedModels}} \cr
#'   \code{decorators()} \tab \code{\link{decorators}} \cr
#'   \code{valueSupport()} \tab \code{\link{valueSupport}} \cr
#'   \code{variateForm()} \tab \code{\link{variateForm}} \cr
#'   \code{type()} \tab \code{\link{type}} \cr
#'   \code{properties()} \tab \code{\link{properties}} \cr
#'   \code{support()} \tab \code{\link{support}} \cr
#'   \code{distrDomain()} \tab \code{\link{distrDomain}} \cr
#'   \code{symmetry()} \tab \code{\link{symmetry}} \cr
#'   \code{sup()}  \tab \code{\link{sup}} \cr
#'   \code{inf()} \tab \code{\link{inf}} \cr
#'   \code{dmax()}  \tab \code{\link{dmax}} \cr
#'   \code{dmin()} \tab \code{\link{dmin}} \cr
#'   \code{skewnessType()} \tab \code{\link{skewnessType}} \cr
#'   \code{kurtosisType()} \tab \code{\link{kurtosisType}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Statistical Methods} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n, simplify = TRUE)} \tab \code{\link{rand}} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{cor()} \tab \code{\link{cor}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Parameter Methods} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(lst, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE)} \tab \code{\link{liesInType}} \cr
#'   \code{liesInDistrDomain(x, all = TRUE)} \tab \code{\link{liesInDistrDomain}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{strprint()} \tab \code{\link{strprint}} \cr
#'   \code{print()} \tab \code{\link[base]{print}} \cr
#'   \code{summary(full = T)} \tab \code{\link{summary.Distribution}} \cr
#'   \code{plot()} \tab Coming Soon. \cr
#'   \code{qqplot()} \tab Coming Soon. \cr
#'   }
#'
#'
NULL
DistributionWrapper <- R6::R6Class("DistributionWrapper", inherit = Distribution, lock_objects = FALSE)
DistributionWrapper$set("public","initialize",function(distlist, prefixParams = TRUE,...){
  if(getR6Class(self) == "DistributionWrapper")
    stop(paste(getR6Class(self), "is an abstract class that can't be initialized."))

  assertDistributionList(distlist)

  lapply(distlist, function(x) x$parameters()$update())
  private$.wrappedModels <- distlist

  if(prefixParams){
    params <- data.table::rbindlist(lapply(distlist, function(x){
      params = x[["parameters"]]()$as.data.table()
      params[,1] = paste(x[["short_name"]],unlist(params[,1]),sep="_")
      return(params)
    }))
    row.names(params) <- NULL
    params <- as.ParameterSet(params)
  } else{
    if(length(distlist) == 1)
      params <- distlist[[1]]$parameters()
    else
      params <- do.call(rbind,lapply(distlist, function(x) x$parameters()))
  }

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
DistributionWrapper$set("public","setParameterValue",function(lst, error = "warn"){
  for(i in 1:length(lst)){
    if(grepl("_",lst[[i]],fixed = T)){
      id = names(lst)[[i]]
      underscore = gregexpr("_",id,fixed=T)[[1]][1]
      model = substr(id,1,underscore-1)
      parameter = substr(id,underscore+1,1000)

      value = lst[[i]]
      newlst = list(value)
      names(newlst) = parameter
    } else{
      model = self$wrappedModels()[[1]]$short_name
      newlst = lst
    }
    self$wrappedModels(model)$setParameterValue(newlst, error)
  }
  rm(i)

  params <- data.table::rbindlist(lapply(self$wrappedModels(), function(x){
    params = x[["parameters"]]()$as.data.table()
    params[,1] = paste(x[["short_name"]],unlist(params[,1]),sep="_")
    return(params)
  }))
  row.names(params) <- NULL
  private$.parameters <- as.ParameterSet(params)

  private$.properties$support <- do.call(product,lapply(self$wrappedModels(),function(x) x$support()))

  invisible(self)
})
