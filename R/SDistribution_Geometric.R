#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
# Geometric Distribution Documentation
#-------------------------------------------------------------
#' @title Geometric Distribution
#'
#' @description Mathematical and statistical functions for the Geometric distribution parameterised
#' with prob or \eqn{qprob = 1 - prob}. The Geometric distribution for
#' the number of X trials before one success is defined by
#' the pmf,
#' \deqn{f(x) = (1 - p)^{k-1}p}
#' where x is the number of trials needed to get one success and \eqn{0 < p < 1} is the probability
#' of success.
#'
#' Alternatively the Geometric distribution can also be defined by the pmf,
#' \deqn{f(x) = (1 - p)^{k}p}
#' where x is the number of failures before the first success and \eqn{0 < p \le 1} is the probability
#' of success.
#'
#' @details The Geometric Distribution is parameterised by default with probability of success = 0.5
#' and such that \eqn{x} is the number of failures before the first success, as this is the implementation
#' in R stats. Once constructed, the type of Geometric distribution cannot be changed or updated as parameter,
#' only the probability of success/failure.
#'
#' @name Geometric
#'
#' @section Constructor: Geometric$new(prob = 0.5, qprob = NULL, trials = FALSE, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{prob} \tab numeric \tab probability of success. \cr
#' \code{qprob} \tab numeric \tab probability of failure. \cr
#' \code{trials} \tab logical \tab number of trials or failures, see details. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: The Geometric distribution is parameterised with probability of success,
#' prob = 0.5 by default. The logical parameter \code{trials} determines which Geometric distribution
#' is constructed and cannot be changed after construction. If \code{trials} is TRUE then
#' the Geometric distribution that calculates the probability of first success is after \eqn{x} trials.
#' Otherwise the Geometric distribution calculates probability of \eqn{y} failures before the first success.
#' Mathematically these are related by \eqn{Y = X - 1}.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
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
#' x <- Geometric$new() # Default is prob = 0.5 and number of failures before first success
#'
#' # Update parameters
#' x$setParameterValue(list(qprob = 0.2))  # When any parameter is updated, all others are too!
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
# Geometric Distribution Definition
#-------------------------------------------------------------
Geometric <-  R6::R6Class("Geometric", inherit = SDistribution, lock_objects = F)

Geometric$set("private",".trails",NULL)

Geometric$set("public","name","Geometric")
Geometric$set("public","short_name","Geom")
Geometric$set("public","traits",list(type = PosIntegers$new(zero = T),
                                       valueSupport = "discrete",
                                       variateForm = "univariate"))
Geometric$set("public","package","stats")

Geometric$set("public","mean",function(){
    if(private$.trials)
        return(1/self$getParameterValue("prob"))
    else
        return((1-self$getParameterValue("prob"))/self$getParameterValue("prob"))
})
Geometric$set("public","var",function(){
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
    self$setParameterValue(list(prob = prob, qprob = qprob))

    if(!trials){
        pdf <- function(x1) dgeom(x1, self$getParameterValue("prob"))
        cdf <- function(x1) pgeom(x1, self$getParameterValue("prob"))
        quantile <- function(p) qgeom(p, self$getParameterValue("prob"))
        rand <- function(n) rgeom(n, self$getParameterValue("prob"))
        support <- PosIntegers$new(zero = T)
        description = "Geometric (Failures) Probability Distribution."
    } else {
        pdf <- function(x1) dgeom(x1+1, self$getParameterValue("prob"))
        cdf <- function(x1) pgeom(x1+1, self$getParameterValue("prob"))
        quantile <- function(p) qgeom(p, self$getParameterValue("prob"))+1
        rand <- function(n) rgeom(n, self$getParameterValue("prob"))
        support <- PosIntegers$new(zero = F)
        description = "Geometric (Trials) Probability Distribution."
    }



    super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile,
                     rand = rand, support = support, distrDomain = PosIntegers$new(zero = T),
                     symmetric  = FALSE, description = description)

    invisible(self)

})


