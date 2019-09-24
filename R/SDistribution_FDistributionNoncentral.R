#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Noncentral F Distribution Documentation
#-------------------------------------------------------------
#' @name FDistributionNoncentral
#' @author Jordan Deenichin
#' @template SDist
#' @templateVar ClassName FDistributionNoncentral
#' @templateVar DistName Noncentral F
#' @templateVar uses in ANOVA testing and is the ratio of scaled Chi-Squared distributions
#' @templateVar params two degrees of freedom parameters, \eqn{\mu, \nu}, and location, \eqn{\lambda},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x) = \sum_{r=0}^{\infty} ((exp(-\lambda/2)(\lambda/2)^r)/(B(\nu/2, \mu/2+r)r!))(\mu/\nu)^{\mu/2+r}(\nu/(\nu+x\mu))^{(\mu+\nu)/2+r}x^{\mu/2-1+r}}
#' @templateVar paramsupport \eqn{\mu, \nu > 0, \lambda \ge 0}
#' @templateVar distsupport the Positive Reals
#' @templateVar omittedVars \code{skewness}, \code{kurtosis}, \code{entropy}, \code{mode}, \code{mgf} and \code{cf}
#' @templateVar constructor df1 = 1, df2 = 1, location = 0
#' @templateVar arg1 \code{df1, df2} \tab numeric \tab degrees of freedom. \cr
#' @templateVar arg2 \code{location} \tab numeric \tab location (ncp in rstats). \cr
#' @templateVar constructorDets \code{df1} and \code{df2} as positive numerics, \code{location} as non-negative numeric.
#' @templateVar additionalSeeAlso \code{\link{Normal}}, \code{\link{ChiSquared}} and \code{\link{FDistribution}} for the Normal, Chi-Squared and central F distributions.
#'
#' @examples
#' x <- FDistributionNoncentral$new(df1 = 1, df2 = 3, location = 2)
#'
#' # Update parameters
#' x$setParameterValue(df2 = 10)
#' x$parameters()
#'
#' # d/p/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# Noncentral F Distribution Definition
#-------------------------------------------------------------
FDistributionNoncentral <- R6::R6Class("FDistributionNoncentral", inherit = SDistribution, lock_objects = FALSE)
FDistributionNoncentral$set("public", "name", "FDistributionNoncentral")
FDistributionNoncentral$set("public", "short_name", "FNC")
FDistributionNoncentral$set("public", "description", "F Probability Distribution")
FDistributionNoncentral$set("public", "package", "stats")

FDistributionNoncentral$set("public", "mean", function(){
  if(self$getParameterValue("df2") > 2){
    df1 <- self$getParameterValue("df1")
    df2 <- self$getParameterValue("df2")
    loc <- self$getParameterValue("location")
    return(df2*(df1 + loc)/(df1*(df2 - 2)))
  }
  else
    return(NaN)
})
FDistributionNoncentral$set("public", "variance", function(){
  if(self$getParameterValue("df2") > 4){
    df1 <- self$getParameterValue("df1")
    df2 <- self$getParameterValue("df2")
    loc <- self$getParameterValue("location")
    return(2*(df2/df1)^2*((df1 + loc)^2 + (df1 + 2*loc)*(df2 - 2))/((df2 - 2)^2*(df2 - 4)))
  }
  else
    return(NaN)
})

FDistributionNoncentral$set("public", "setParameterValue",function(..., lst = NULL, error = "warn"){
  super$setParameterValue(..., lst = lst, error = error)
  if (self$getParameterValue("df1") == 1)
    private$.properties$support <- PosReals$new(zero = FALSE)
  else
    private$.properties$support <- PosReals$new(zero = TRUE)
  invisible(self)
})
FDistributionNoncentral$set("private", ".getRefParams", function(paramlst){
  lst = list()
  if (!is.null(paramlst$df1)) lst = c(lst, list(df1 = paramlst$df1))
  if (!is.null(paramlst$df2)) lst = c(lst, list(df2 = paramlst$df2))
  if (!is.null(paramlst$location)) lst = c(lst, list(location = paramlst$location))
  return(lst)
})

FDistributionNoncentral$set("public", "initialize", function(df1 = 1, df2 = 1, location = 0, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, df1, df2, location, verbose)
  self$setParameterValue(df1 = df1, df2 = df2, location = location)

  pdf <- function(x1) df(x1, df1, df2, location)
  cdf <- function(x1) pf(x1, df1, df2, location)
  quantile <- function(p) qf(p, df1, df2, location)
  rand <- function(n) rf(n, df1, df2, location)

  if (df1 == 1)
    support <- PosReals$new(zero = FALSE)
  else
    support <- PosReals$new(zero = TRUE)

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                   rand = rand, support = support,
                   symmetric = FALSE,type = PosReals$new(zero = TRUE),
                   valueSupport = "continuous",
                   variateForm = "univariate")
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "FNC", ClassName = "FDistributionNoncentral",
                                                     Type = "\u211D+", ValueSupport = "continuous",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
