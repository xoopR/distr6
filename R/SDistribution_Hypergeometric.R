#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Hypergeometric Distribution Documentation
#-------------------------------------------------------------
#' @title Hypergeometric Distribution
#'
#' @description Mathematical and statistical functions for the Hypergeometric distribution parameterised
#' with population size N, the number of success states in the population K and the number of draws n(i.e. quantity drawn in each trial).
#' The  Hypergeometric distribution is defined by the pmf,
#' \deqn{f(x) = C(K, x)*C(N-K,n-x)/C(N,n)}
#' where \eqn{N = 0,1,2,\ldots} is the population size parameter, \eqn{K = 0,1,2,\ldots} is the success parameter and
#' \eqn{n = 0,1,2,\ldots} is the draws parameter.\eqn{C(a,b)} is the combination (or binomial coefficient) function.
#'
#' @name Hypergeometric
#' @section Constructor: Hypergeometric$new(size = 10, success = 5, draws = 2,decorators = NULL, verbose = FALSE)
#' 
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab  population size. \cr
#' \code{success} \tab numeric \tab number of success states in the population. \cr
#' \code{draws} \tab numeric \tab number of draws. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @inheritSection Distribution Public Variables
#' @inheritSection Distribution Accessor Methods
#' @inheritSection Distribution p/d/q/r Methods
#' @inheritSection Distribution Parameter Methods
#' @inheritSection Distribution Validation Methods
#' @inheritSection Distribution Representation Methods
#'
#' @section Statistical Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{mean()} \tab \code{\link{mean.Distribution}} \cr
#'   \code{var()} \tab \code{\link{var}} \cr
#'   \code{skewness()} \tab \code{\link{skewness}} \cr
#'   \code{kurtosis(excess = TRUE)} \tab \code{\link{kurtosis}} \cr
#'   \code{entropy(base = 2)} \tab \code{\link{entropy}} \cr
#'   \code{mgf(t)} \tab \code{\link{mgf}} \cr
#'   \code{pgf(z)} \tab \code{\link{pgf}} \cr
#'   \code{cf(t)} \tab \code{\link{cf}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   }
#'
#' @export
NULL
#-------------------------------------------------------------
# Hypergeometric Distribution Definition
#-------------------------------------------------------------
Hypergeometric <- R6::R6Class("Hypergeometric", inherit = SDistribution, lock_objects = FALSE)
Hypergeometric$set("public", "name", "Hypergeometric")
Hypergeometric$set("public", "short_name", "HyperGeom")
Hypergeometric$set("public", "traits", list(type = PosIntegers$new(zero = TRUE),
                                        valueSupport = "discrete",
                                        variateForm = "univariate"))
Hypergeometric$set("public", "description", "ChiSquared Probability Distribution")

Hypergeometric$set("public", "mean", function(){
    return(self$getParameterValue("draws")*self$getParameterValue("success")/self$getParameterValue("size"))
})

Hypergeometric$set("public","var",function(){
    self$getParameterValue("draws")*self$getParameterValue("success")*(self$getParameterValue("size")-self$getParameterValue("success"))*(self$getParameterValue("size")-self$getParameterValue("draws"))/(self$getParameterValue("size")*self$getParameterValue("size")*(self$getParameterValue("size")-1))
})

Hypergeometric$set("public","skewness",function(){
    ( (self$getParameterValue("size")-2*self$getParameterValue("success"))*((self$getParameterValue("size")-1)^0.5)*(self$getParameterValue("size") - 2*self$getParameterValue("draws")) )/( ((self$getParameterValue("draws")*self$getParameterValue("success")*(self$getParameterValue("size")-self$getParameterValue("success"))*(self$getParameterValue("size")-self$getParameterValue("draws")))^0.5)*(self$getParameterValue("size")-2) ) 
})

Hypergeometric$set("public","kurtosis",function(excess = TRUE){
    exkurtosis = ( (self$getParameterValue("size")-1)*((self$getParameterValue("size"))^2)*((self$getParameterValue("size")*(self$getParameterValue("size")+1)) - 6*self$getParameterValue("success")*(self$getParameterValue("size")-self$getParameterValue("success")) - 6*self$getParameterValue("draws")*(self$getParameterValue("size")-self$getParameterValue("draws")) ) + 6*self$getParameterValue("draws")*self$getParameterValue("success")*(self$getParameterValue("size")-self$getParameterValue("success"))*(self$getParameterValue("size")-self$getParameterValue("draws"))*(5*self$getParameterValue("size")-6) )/(6*self$getParameterValue("draws")*self$getParameterValue("success")*(self$getParameterValue("size")-self$getParameterValue("success"))*(self$getParameterValue("size")-self$getParameterValue("draws"))*(5*self$getParameterValue("size")-6)) 
    if(excess)
        return(exkurtosis)
    else
        return(exkurtosis + 3)
})

Hypergeometric$set("public","setParameterValue",function(lst, error = "warn"){
    super$setParameterValue(lst, error)
    private$.properties$support <- Set$new(0:self$getParameterValue("size"))
    private$.properties$support <- Set$new(0:self$getParameterValue("success"))
    private$.properties$support <- Set$new(0:self$getParameterValue("draws"))
    
})

Hypergeometric$set("private",".getRefParams", function(paramlst){
    lst = list()
    if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
    if(!is.null(paramlst$success)) lst = c(lst, list(success = paramlst$success))
    if(!is.null(paramlst$draws)) lst = c(lst, list(draws = paramlst$draws))
    return(lst)
})

Hypergeometric$set("public","initialize",function(size = 10, success = 5, draws = 2, decorators = NULL, verbose = FALSE){
    
    private$.parameters <- getParameterSet(self, size, success, draws, verbose)
    self$setParameterValue(list(size = size, success=success, draws = draws))
    
    pdf = function(x1) dhyper(x1, self$getParameterValue("success"), self$getParameterValue("size")-self$getParameterValue("success"),self$getParameterValue("draws"))
    cdf = function(x1) phyper(x1, self$getParameterValue("success"), self$getParameterValue("size")-self$getParameterValue("success"),self$getParameterValue("draws"))
    quantile = function(p) qhyper(p, self$getParameterValue("success"), self$getParameterValue("size")-self$getParameterValue("success"),self$getParameterValue("draws"))
    rand = function(n) dbinom(n, self$getParameterValue("success"), self$getParameterValue("size")-self$getParameterValue("success"),self$getParameterValue("draws"))
    
    super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                     rand = rand, support = Set$new(0:size), distrDomain = PosIntegers$new(zero = T),
                     symmetric = FALSE)
    invisible(self)
})


