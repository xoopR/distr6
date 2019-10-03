#-------------------------------------------------------------
# SDistribution Documentation
#-------------------------------------------------------------
#' @title Abstract Special Distribution Class
#'
#' @description Abstract class that cannot be constructed directly.
#'
#' @name SDistribution
#'
#' @section Public Variables:
#'  \tabular{ll}{
#'   \strong{Variable} \tab \strong{Return} \cr
#'   \code{name} \tab Name of distribution. \cr
#'   \code{short_name} \tab Id of distribution. \cr
#'   \code{description} \tab Brief description of distribution. \cr
#'   \code{package} \tab The package d/p/q/r are implemented in.
#'  }
#'
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
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
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Statistical Methods} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n, simplify = TRUE)} \tab \code{\link{rand}} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{variance()} \tab \code{\link{variance}} \cr
#'   \code{stdev()} \tab \code{\link{stdev}} \cr
#'   \code{prec()} \tab \code{\link{prec}} \cr
#'   \code{cor()} \tab \code{\link{cor}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Parameter Methods} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(..., lst = NULL, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInType}} \cr
#'
#'   \tab \cr \tab \cr \tab \cr
#'
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{strprint(n = 2)} \tab \code{\link{strprint}} \cr
#'   \code{print(n = 2)} \tab \code{\link[base]{print}} \cr
#'   \code{summary(full = T)} \tab \code{\link{summary.Distribution}} \cr
#'   }
#'
#' @section Active Bindings:
#'  \tabular{ll}{
#'   \strong{Active Binding} \tab \strong{Link} \cr
#'   \code{isPdf} \tab \code{\link{isPdf}} \cr
#'   \code{isCdf} \tab \code{\link{isCdf}} \cr
#'   \code{isQuantile} \tab \code{\link{isQuantile}} \cr
#'   \code{isRand} \tab \code{\link{isRand}} \cr
#'   }
#'
#' @return Returns error. Abstract classes cannot be constructed directly.
#'
#' @export
NULL
#-------------------------------------------------------------
# SDistribution Definition
#-------------------------------------------------------------
SDistribution <- R6::R6Class("SDistribution", inherit = Distribution)
SDistribution$set("public","initialize",function(...){
  if(getR6Class(self) == "SDistribution")
    stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Use listDistributions()
    to see the probability distributions currently implemented in distr6."))

  super$initialize(...)
})

SDistribution$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  lst <- private$.getRefParams(lst)
  super$setParameterValue(lst = lst, error = error)
  invisible(self)
})
SDistribution$set("public","package",NULL)
# SDistribution$set("public","pgf",function(...){
#   return(NaN)
# })
