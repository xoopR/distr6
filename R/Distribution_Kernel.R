#' @include SetInterval_SpecialSet.R
#-------------------------------------------------------------
# Kernel Documentation
#-------------------------------------------------------------
#' @title Abstract Kernel Class
#'
#' @description Abstract class that cannot be constructed directly. See \code{listKernels} for a list of
#' implemented kernels.
#'
#' @name Kernel
#'
#' @seealso \code{\link{listKernels}}
#'
#' @inheritSection SDistribution Public Variables
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
#'   \code{mode()} \tab \code{\link{mode}} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   \code{correlation()} \tab \code{\link{correlation}} \cr
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
Kernel <- R6::R6Class("Kernel", inherit = Distribution)
Kernel$set("public","initialize",function(...){
  if(getR6Class(self) == "Kernel")
    stop(paste0(getR6Class(self), " is an abstract class that can't be initialized. Use listKernels()
to see the kernels currently implemented in distr6, or Distribution$new() to construct a custom Kernel."))

  super$initialize(...)
})
Kernel$set("public","package","distr6")
Kernel$set("private",".type","symmetric")
Kernel$set("public","traits",function(){
  return(list(type = Reals$new(),
              valueSupport = "continuous",
              variateForm = "univariate"))
})
Kernel$set("public","mode",function(){
  return(0)
})
Kernel$set("public","mean",function(){
  return(0)
})
Kernel$set("public","median",function(){
  return(0)
})
Kernel$set("public","rand",function(n, simplify = TRUE){
  if(length(n) > 1)
    n <- length(n)

  rand <- self$quantile(runif(n))

  if(simplify)
    return(rand)
  else{
    rand = data.table::data.table(rand)
    colnames(rand) = self$short_name
    return(rand)
  }
})

