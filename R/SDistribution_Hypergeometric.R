#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Hypergeometric Distribution Documentation
#-------------------------------------------------------------
#' @name Hypergeometric
#' @template SDist
#' @templateVar ClassName Hypergeometric
#' @templateVar DistName Hypergeometric
#' @templateVar uses to model the number of successes out of a population containing a known number of possible successes, for example the number of red balls from an urn or red, blue and yellow balls
#' @templateVar params population size, \eqn{N}, number of possible successes, \eqn{K}, and number of draws from the distribution, \eqn{n},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = C(K, x)C(N-K,n-x)/C(N,n)}
#' @templateVar paramsupport \eqn{N = \{0,1,2,\ldots\}}{N = {0,1,2,\ldots}}, \eqn{n, K = \{0,1,2,\ldots,N\}}{n, K = {0,1,2,\ldots,N}} and \eqn{C(a,b)} is the combination (or binomial coefficient) function
#' @templateVar distsupport \eqn{\{max(0, n + K - N),...,min(n,K)\}}{{max(0, n + K - N),...,min(n,K)}}
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar constructor size = 10, successes = 5, failures = NULL, draws = 2
#' @templateVar arg1 \code{size} \tab numeric \tab  population size. \cr
#' @templateVar arg2 \code{successes} \tab numeric \tab number of population successes. \cr
#' @templateVar arg3 \code{failures} \tab numeric \tab number of population failures. \cr
#' @templateVar arg4 \code{draws} \tab numeric \tab number of draws. \cr
#' @templateVar constructorDets \code{size} and \code{draws} as positive whole numbers, and either \code{successes} or \code{failures} as positive whole numbers. These are related via, \deqn{failures = size - successes} If \code{failures} is given then \code{successes} is ignored.
#'
#' @examples
#' Hypergeometric$new(size = 10, successes = 7, draws = 5)
#' Hypergeometric$new(size = 10, failures = 3, draws = 5)
#'
#' # Default is size = 50, successes = 5, draws = 10
#' x = Hypergeometric$new(verbose = TRUE)
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(failures = 10)
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
# Hypergeometric Distribution Definition
#-------------------------------------------------------------
Hypergeometric <- R6::R6Class("Hypergeometric", inherit = SDistribution, lock_objects = FALSE)
Hypergeometric$set("public", "name", "Hypergeometric")
Hypergeometric$set("public", "short_name", "Hyper")
Hypergeometric$set("public", "description", "Hypergeometric Probability Distribution")
Hypergeometric$set("public","package","stats")

Hypergeometric$set("public", "mean", function(){
    return(self$getParameterValue("draws")*self$getParameterValue("successes")/self$getParameterValue("size"))
})
Hypergeometric$set("public","mode",function(){
    draws <- self$getParameterValue("draws")
    successes <- self$getParameterValue("successes")
    size <- self$getParameterValue("size")
    return(floor(((draws + 1)*(successes + 1))/(size+2)))
})
Hypergeometric$set("public","variance",function(){
    draws <- self$getParameterValue("draws")
    successes <- self$getParameterValue("successes")
    size <- self$getParameterValue("size")
    return((draws*successes*(size-successes)*(size-draws))/(size^2*(size-1))
    )
})
Hypergeometric$set("public","skewness",function(){
    draws <- self$getParameterValue("draws")
    successes <- self$getParameterValue("successes")
    size <- self$getParameterValue("size")
    return(((size-2*successes)*((size-1)^0.5)*(size - 2*draws))/
        (((draws*successes*(size-successes)*(size-draws))^0.5)*(size-2)))
})
Hypergeometric$set("public","kurtosis",function(excess = TRUE){
    draws <- self$getParameterValue("draws")
    successes <- self$getParameterValue("successes")
    size <- self$getParameterValue("size")

    exkurtosis = ((size-1)*(size^2)*((size*(size+1)) - 6*successes*(size-successes) -
                                           6*draws*(size-draws)) + 6*draws*successes*(size-successes)*
                      (size-draws)*(5*size-6))/(draws*successes*(size-successes)*(size-draws)*(size-2)*
                                                    (size-3))

    if(excess)
        return(exkurtosis)
    else
        return(exkurtosis + 3)
})

Hypergeometric$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
    super$setParameterValue(..., lst = lst, error = error)
    size = self$getParameterValue("size")

    private$.properties$support <- Set$new(max(0, self$getParameterValue("draws") +
                                                   self$getParameterValue("successes") - size):
                                               min(self$getParameterValue("draws"),
                                                   self$getParameterValue("successes")))

    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(successes = Set$new(0:size)))
    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(draws = Set$new(0:size)))
    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(failures = Set$new(0:size)))
    invisible(self)
})

Hypergeometric$set("private",".getRefParams", function(paramlst){
    lst = list()
    if(!is.null(paramlst$size)) lst = c(lst, list(size = paramlst$size))
    if(!is.null(paramlst$successes)) lst = c(lst, list(successes = paramlst$successes))
    if(!is.null(paramlst$failures)){
        if(!is.null(paramlst$size)) lst = c(lst, list(successes = paramlst$size-paramlst$failures))
        else lst = c(lst, list(successes = self$getParameterValue("size")-paramlst$failures))
    }
    if(!is.null(paramlst$draws)) lst = c(lst, list(draws = paramlst$draws))

    return(lst)
})

Hypergeometric$set("public","initialize",function(size = 50, successes = 5, failures = NULL, draws = 10,
                                                  decorators = NULL, verbose = FALSE){

    private$.parameters <- getParameterSet(self, size, successes, failures, draws, verbose)
    self$setParameterValue(size = size, successes=successes, failures = failures, draws = draws)

    pdf = function(x1) dhyper(x1, self$getParameterValue("successes"),
                              self$getParameterValue("failures"),
                              self$getParameterValue("draws"))
    cdf = function(x1) phyper(x1, self$getParameterValue("successes"),
                              self$getParameterValue("failures"),
                              self$getParameterValue("draws"))
    quantile = function(p) qhyper(p, self$getParameterValue("successes"),
                                  self$getParameterValue("failures"),
                                  self$getParameterValue("draws"))
    rand = function(n) rhyper(n, self$getParameterValue("successes"),
                              self$getParameterValue("failures"),
                              self$getParameterValue("draws"))

    support <- Set$new(max(0, draws + successes - size):min(draws,successes))

    super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                     rand = rand, support = support,
                     symmetric = FALSE,type = Naturals$new(),
                     valueSupport = "discrete",
                     variateForm = "univariate")
    invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Hyper", ClassName = "Hypergeometric",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))

