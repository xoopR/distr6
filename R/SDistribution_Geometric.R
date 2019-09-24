#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Geometric Distribution Documentation
#-------------------------------------------------------------
#' @name Geometric
#' @template SDist
#' @templateVar ClassName Geometric
#' @templateVar DistName Geometric
#' @templateVar uses to model the number of trials (or number of failures) before the first success
#' @templateVar params probability of success, \eqn{p},
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x) = (1 - p)^{k-1}p}
#' @templateVar paramsupport \eqn{p \epsilon [0,1]}
#' @templateVar distsupport the Naturals (zero is included if modelling number of failures before success).
#' @templateVar additionalDetails The Geometric distribution is used to either refer to modelling the number of trials or number of failures before the first success.
#' @templateVar constructor prob = 0.5, qprob = NULL, trials = FALSE
#' @templateVar arg1 \code{prob} \tab numeric \tab probability of success. \cr
#' @templateVar arg2 \code{qprob} \tab numeric \tab probability of failure. \cr
#' @templateVar arg3 \code{trials} \tab logical \tab number of trials or failures, see details. \cr
#' @templateVar constructorDets \code{prob} or \code{qprob} as a number between 0 and 1. These are related via, \deqn{qprob = 1 - prob} If \code{qprob} is given then {prob is ignored}. \cr\cr The logical parameter \code{trials} determines which Geometric distribution is constructed and cannot be changed after construction. If \code{trials} is TRUE then the Geometric distribution that models the number of trials, \eqn{x}, before the first success is constructed. Otherwise the Geometric distribution calculates the probability of \eqn{y} failures before the first success. Mathematically these are related by \eqn{Y = X - 1}.
#'
#' @examples
#' # Different parameterisations
#' Geometric$new(prob = 0.2)
#' Geometric$new(qprob = 0.7)
#'
#' # Different forms of the distribution
#' Geometric$new(trials = TRUE) # Number of trials before first success
#' Geometric$new(trials = FALSE) # Number of failures before first success
#'
#' # Use description to see which form is used
#' Geometric$new(trials = TRUE)$description
#' Geometric$new(trials = FALSE)$description
#'
#' # Default is prob = 0.5 and number of failures before first success
#' x <- Geometric$new()
#'
#' # Update parameters
#' # When any parameter is updated, all others are too!
#' x$setParameterValue(qprob = 0.2)
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
# Geometric Distribution Definition
#-------------------------------------------------------------
Geometric <-  R6::R6Class("Geometric", inherit = SDistribution, lock_objects = F)

Geometric$set("private",".trials",NULL)

Geometric$set("public","name","Geometric")
Geometric$set("public","short_name","Geom")
Geometric$set("public","package","stats")

Geometric$set("public","mean",function(){
    if(private$.trials)
        return(1/self$getParameterValue("prob"))
    else
        return((1-self$getParameterValue("prob"))/self$getParameterValue("prob"))
})
Geometric$set("public","variance",function(){
   return((1-self$getParameterValue("prob"))/(self$getParameterValue("prob")^2))
})
Geometric$set("public","skewness",function(){
    return((2-self$getParameterValue("prob"))/sqrt(1-self$getParameterValue("prob")))
})
Geometric$set("public","kurtosis",function(excess = TRUE){
    exkurtosis = 6 + (self$getParameterValue("prob")^2/(1-self$getParameterValue("prob")))
    if(excess)
        return(exkurtosis)
    else
        return(exkurtosis + 3)
})
Geometric$set("public","entropy",function(base = 2){
    prob <- self$getParameterValue("prob")
    return(((-(1-prob)*log(1-prob,base))-(prob*log(prob,base)))/prob)
})
Geometric$set("public", "mgf", function(t){
    if(private$.trials){
        if(t < -log(1-self$getParameterValue("prob")))
            return((self$getParameterValue("prob")*exp(t))/(1-(1-self$getParameterValue("prob"))*exp(t)))
        else
            return(NaN)
    } else
        return((self$getParameterValue("prob"))/(1-(1-self$getParameterValue("prob"))*exp(t)))
})
Geometric$set("public", "cf", function(t){
    if(private$.trials)
        return((self$getParameterValue("prob")*exp(1i*t))/(1-(1-self$getParameterValue("prob"))*exp(1i*t)))
    else
        return((self$getParameterValue("prob"))/(1-(1-self$getParameterValue("prob"))*exp(1i*t)))
})
Geometric$set("public","pgf",function(z){
    if(private$.trials)
        return((self$getParameterValue("prob") * z)/(1-z*self$getParameterValue("qprob")))
    else
        return(self$getParameterValue("prob")/(1-z*self$getParameterValue("qprob")))
})
Geometric$set("public","mode",function(){
    if(private$.trials)
        return(1)
    else
        return(0)
})

Geometric$set("private",".getRefParams", function(paramlst){
    lst = list()
    if(!is.null(paramlst$prob)) lst = c(lst, list(prob = paramlst$prob))
    if(!is.null(paramlst$qprob)) lst = c(lst, list(prob = 1 - paramlst$qprob))
    return(lst)
})

Geometric$set("public","initialize",function(prob = 0.5, qprob = NULL, trials = FALSE, decorators = NULL,
                                             verbose = FALSE,...){

    checkmate::assertLogical(trials)
    private$.trials <- trials
    private$.parameters <- getParameterSet(x=self, prob=prob, qprob=qprob, trials=trials, verbose=verbose)
    self$setParameterValue(prob = prob, qprob = qprob)

    if(!trials){
        pdf <- function(x1) dgeom(x1, self$getParameterValue("prob"))
        cdf <- function(x1) pgeom(x1, self$getParameterValue("prob"))
        quantile <- function(p) qgeom(p, self$getParameterValue("prob"))
        rand <- function(n) rgeom(n, self$getParameterValue("prob"))
        support <- Naturals$new()
        description = "Geometric (Failures) Probability Distribution."
    } else {
        pdf <- function(x1) dgeom(x1+1, self$getParameterValue("prob"))
        cdf <- function(x1) pgeom(x1+1, self$getParameterValue("prob"))
        quantile <- function(p) qgeom(p, self$getParameterValue("prob"))+1
        rand <- function(n) rgeom(n, self$getParameterValue("prob"))
        support <- PosNaturals$new()
        description = "Geometric (Trials) Probability Distribution."
    }

    super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                     rand = rand, support = support,
                     symmetric  = FALSE, description = description,type = Naturals$new(),
                     valueSupport = "discrete",
                     variateForm = "univariate")

    invisible(self)

})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Geom", ClassName = "Geometric",
                                                     Type = "\u21150", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "stats"))
