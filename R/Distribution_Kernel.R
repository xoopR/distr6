#' @include SetInterval_SpecialSet.R
#-------------------------------------------------------------
# Kernel Documentation
#-------------------------------------------------------------
#' @title Abstract Kernel Class
#'
#' @description Abstract class that cannot be constructed directly.
#'
#' @name Kernel
#'
#' @section Public Variables:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab Name of kernel. \cr
#'   \code{short_name} \tab Id of kernel. \cr
#'  }
#'
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
#'   \code{decorators()} \tab \code{\link{decorators}} \cr
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
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
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
#' @export
NULL
Kernel <- R6::R6Class("Kernel", inherit = Distribution)
Kernel$set("public","traits",list(type = Reals$new(),
                                      valueSupport = "continuous",
                                      variateForm = "univariate"))
Kernel$set("public","package","distr6")
Kernel$set("private",".type","symmetric")
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

  return(self$quantile(runif(n)))
})

#' @title Squared Probability Density Function 2-Norm
#' @name squared2Norm
#' @description The squared 2-norm of the Kernel pdf evaluated over the whole support.
#'
#' @usage squared2Norm(object)
#' @section R6 Usage: $squared2Norm()
#'
#' @param object Distribution.
#'
#' @details The squared 2-norm of the pdf is defined by
#' \deqn{\int (f_X(u))^2 du}
#' where X is the Kernel and \eqn{f_X} is its pdf.
#'
#' @export
NULL
Kernel$set("public","squared2Norm",function() return(NULL))
