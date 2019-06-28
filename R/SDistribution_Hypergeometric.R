#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Hypergeometric Distribution Documentation
#-------------------------------------------------------------
#' @title Hypergeometric Distribution
#'
#' @description Mathematical and statistical functions for the Hypergeometric distribution parameterised
#' with population size, number of successes (or failures) in the population and number of draws. It is defined by
#' the pmf,
#' \deqn{f(x) = C(K, x)*C(N-K,n-x)/C(N,n)}
#' where \eqn{N = 0,1,2,\ldots} is the population size parameter, \eqn{K = 0,1,2,\ldots} is the number
#' of successes parameter and \eqn{n = 0,1,2,\ldots} is the number of draws parameter. \eqn{C(a,b)}
#' is the combination (or binomial coefficient) function.
#'
#' @details \code{mgf} and \code{cf} are omitted as no closed form analytic expression could be found.
#'
#' @name Hypergeometric
#' @section Constructor: Hypergeometric$new(size = 10, successes = 5, failures = 5, draws = 2, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{size} \tab numeric \tab  population size. \cr
#' \code{successes} \tab numeric \tab number of population successes. \cr
#' \code{failures} \tab numeric \tab number of population failures. \cr
#' \code{draws} \tab numeric \tab number of draws. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' Hypergeometric$new(size = 10, successes = 7, draws = 5)
#' Hypergeometric$new(size = 10, failures = 3, draws = 5)
#'
#' x = Hypergeometric$new(verbose = TRUE) # Default is size = 50, successes = 5, draws = 10
#'
#' # Update parameters
#' x$setParameterValue(list(failures = 10)) # When any parameter is updated, all others are too!
#' x$parameters()
#'
#' # p/d/q/r
#' x$pdf(5)
#' x$cdf(5)
#' x$quantile(0.42)
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$var()
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
Hypergeometric$set("public", "short_name", "Hypergeom")
Hypergeometric$set("public", "traits", list(type = PosIntegers$new(zero = TRUE),
                                        valueSupport = "discrete",
                                        variateForm = "univariate"))
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
Hypergeometric$set("public","var",function(){
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

Hypergeometric$set("public","setParameterValue",function(lst, error = "warn"){
    super$setParameterValue(lst, error)
    size = self$getParameterValue("size")

    private$.properties$support <- Set$new(max(0, self$getParameterValue("draws") +
                                                   self$getParameterValue("successes") - size):
                                               min(self$getParameterValue("draws"),
                                                   self$getParameterValue("successes")))

    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(successes = Set$new(0:size)))
    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(draws = Set$new(0:size)))
    self$parameters()$.__enclos_env__$private$.SetParameterSupport(list(failures = Set$new(0:size)))
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
    self$setParameterValue(list(size = size, successes=successes, failures = failures, draws = draws))

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
                     rand = rand, support = support, distrDomain = PosIntegers$new(zero = T),
                     symmetric = FALSE)
    invisible(self)
})


