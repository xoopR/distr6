#-------------------------------------------------------------
# Distribution R6Class Definition
#-------------------------------------------------------------
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#
#' @return \code{Distribution$new} constructs an R6 object of class Distribution.
#' @name Distribution
#'
#' @param name full name of distribution.
#' @param short_name short name to identify distribution.
#' @param type R6 Set; the scientific type.
#' @param support R6 Set; distribution support. See Details.
#' @param distrDomain R6 Set; distribution domain See Details.
#' @param symmetric logical; is distribution symmetric?
#' @param pdf function. See Details.
#' @param cdf function. See Details.
#' @param quantile function. See Details.
#' @param rand function. See Details.
#' @param parameters S3 ParameterSet. See Details.
#' @param paramValues list. See Details.
#' @param decorators list of decorators to add in construction.
#' @param valueSupport continuous, discrete, mixture. See Details.
#' @param variateForm univariate, multivariate, matrixvariate. See Details.
#' @param description short description of distribution.
#'
#' @details The primary purpose of the Distribution object is to serve as the parent class
#'   to all other distributions, therefore all methods are approximate numeric calculations
#'   and the user may prefer to utilise decorators to improve accuracy.
#'
#'   \code{type}, \code{support} and \code{distrDomain} should be given as an R6 SetInterval
#'   object. If none are supplied then the set of Reals is taken to be the type, support and domain
#'   of the distribution. If only \code{type} is supplied then this is taken to also be the support
#'   and domain.
#'
#'   By default, missing \code{pdf}, \code{cdf} and \code{quantile} are not automatically imputed.
#'   Use the imputation wrappers (see below) to geenrate these with a selected method.
#'   The \code{rand} function is automatically generated depending on which of the above are supplied.
#'   The generation for this is performed according to the hierarchy: quantile -> rand, cdf -> rand, pdf -> rand.
#'
#'   \code{parameters} should be supplied as a ParameterSet. The distribution parameterisation
#'   is taken to be whichever parameters are flagged as 'settable', any others in the ParameterSet
#'   are automatically updated by a given function. \code{paramValues} is an optional list giving the
#'   values to set the parameters (if not default or given in the ParameterSet.
#'
#'   \code{decorators} is a list of decorators (R6 environments not strings) to decorate the
#'   Distribution with in construction. Decorators can also be added after construction. See
#'   \code{\link{DistributionDecorator}} for more details.
#'
#'   \code{valueSupport} and \code{variateForm} if not given are automatically filled from
#'   \code{type} and \code{support}.
#'
#' @seealso \code{\link{SetInteval}} and \code{\link{SpecialSet}} for details on Sets and
#' Intervals. See \code{\link{makeParameterSet}} for parameter details. See \code{\link{DistributionDecorator}} for
#' Decorator details.
NULL
#-------------------------------------------------------------

#' @export
Distribution <- R6::R6Class("Distribution", lock_objects = FALSE)

#-------------------------------------------------------------
# Distribution Private Methods
#-------------------------------------------------------------
Distribution$set("private",".setWorkingSupport",function(){
  suppressMessages({
    rands = self$rand(20)

    if(self$sup() != Inf)
      newsup = self$sup()
    else{
      newsup = max(rands)
      while(self$pdf(newsup) > .Machine$double.eps) newsup = newsup + 1
      newsup = ceiling(newsup - 1)
    }

    inf = self$inf()
    if(inf != -Inf)
      newinf = inf
    else{
      newinf = min(rands)
      while(self$pdf(newinf) > .Machine$double.eps) newinf = newinf - 1
      newinf = floor(newinf + 1)
    }

    private$.workingSupport <- list(inf = newinf, sup = newsup)
  })
}) # NEEDS TESTING
Distribution$set("private",".getWorkingSupportRange",function(){
  return(private$.workingSupport$inf:private$.workingSupport$sup)
}) # NEEDS TESTING
Distribution$set("private",".genQ2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    return(sapply(1:n, function(x) self$quantile(runif(1))))
  }, list(aComment = "Sampling derived from quantile function"))
}) # NEEDS TESTING
Distribution$set("private",".genC2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    message("Results from numerical inversion may not be exact.")
    return(sapply(1:n, function(x) GoFKernel::inverse(private$.cdf)(runif(1))))
  }, list(aComment = "Sampling derived from cumulative distribution function via inverse transform sampling using `inverse` function from GoFKernel"))
}) # NEEDS TESTING
Distribution$set("private",".genP2R",function(){
  private$.rand <- function(n){}
  formals(private$.rand)$self = self
  body(private$.rand) = substitute({
    COMMENT <- aComment
    if(testDiscrete(self))
      cdf = function(x) sum(self$pdf(self$inf():self$pdf(x)))
    else if(testContinuous(self)){
      message("Results from numerical integration are approximate only.")
      cdf = function(x) integrate(self$pdf, lower = self$inf(), upper = x)$value
    }
    message("Results from numerical inversion may not be exact.")
    return(sapply(1:n,function(x) GoFKernel::inverse(cdf)(runif(1))))
  }, list(aComment = "Sampling derived from numerical approximation of distribution function and inverse transform sampling using `inverse` function from GoFKernel"))
}) # NEEDS TESTING
#-------------------------------------------------------------
# Distribution Public Methods
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name, short_name,
                      type = Reals$new(), support, distrDomain,
                      symmetric = logical(0),
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters, paramValues = NULL,
                      decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL
                      ){

  # Validation checks
  if(missing(short_name)) short_name = name
  checkmate::assertCharacter(c(name, short_name),
                             .var.name = "'name' and 'short_name' must be of class 'character'.")
  checkmate::assert(length(strsplit(short_name,split=" ")[[1]])==1,
                    .var.name = "'short_name' must be one word only.")

  checkmate::assertLogical(symmetric)

  private$.name <- name
  private$.short_name <- short_name
  if(!is.null(description))
    private$.description <- description

  if(missing(support))
    support = type
  if(missing(distrDomain))
    distrDomain = type

  checkmate::assert(inherits(type,"Set"), .var.name = "type should be class 'Set'.")
  checkmate::assert(inherits(support,"Set"), inherits(distrDomain,"Set"),
                    .var.name = "'support' and 'distrDomain' should be class 'Set'.")

  if(!is.null(valueSupport))
    checkmate::assert(valueSupport == "continuous", valueSupport == "discrete",
           valueSupport == "mixture",
           .var.name = "valueSupport should be one of: 'continuous', 'discrete',
           'mixture'.")
  else if(class(support)[[1]] %in% c("Reals","PosReals","NegReals"))
    valueSupport = "continuous"
  else
    valueSupport = "discrete"

  if(!is.null(variateForm))
    checkmate::assert(variateForm == "univariate", variateForm == "multivariate",
           variateForm == "matrixvariate",
           .var.name = "variateForm should be one of: 'univariate', 'multivariate',
           'matrixvariate'.")
  else if(type$dimension() == 1)
    variateForm = "univariate"
  else
    variateForm = "multivariate"

  private$.traits <- c(private$.traits, type = type)
  private$.traits <- c(private$.traits, valueSupport = valueSupport)
  private$.traits <- c(private$.traits, variateForm = variateForm)

  private$.properties <- c(private$.properties, support = support)
  private$.properties <- c(private$.properties, distrDomain = distrDomain)
  private$.properties <- c(private$.properties, symmetric = symmetric)

  if(!is.null(pdf)){
    if(!is.null(formals(pdf)$self))
      formals(pdf)$self = self
    else
      formals(pdf) = c(formals(pdf),list(self=self))
    private$.pdf <- pdf
  } else
    private$.pdf <- function(...){
      warning("Density/mass function is missing.")
      return(NULL)
    }

  if(!is.null(cdf)){
    if(!is.null(formals(cdf)$self))
      formals(cdf)$self = self
    else
      formals(cdf) = c(formals(cdf),list(self=self))
    private$.cdf <- cdf
  } else
    private$.cdf <- function(...){
      warning("Distribution function is missing.")
      return(NULL)
    }

  if(!is.null(quantile)){
    if(!is.null(formals(quantile)$self))
      formals(quantile)$self = self
    else
      formals(quantile) = c(formals(quantile),list(self=self))
    private$.quantile <- quantile
  } else
    private$.quantile <- function(...){
      warning("Quantile function is missing.")
      return(NULL)
    }

  if(!is.null(rand)){
    if(!is.null(formals(rand)$self))
      formals(rand)$self = self
    else
      formals(rand) = c(formals(rand),list(self=self))
    private$.rand <- rand
  } else {
    if(!is.null(quantile))
      private$.genQ2R()
    else if(!is.null(cdf))
      private$.genC2R()
    else if(!is.null(pdf))
      private$.genP2R()
  }

  if(!missing(parameters)){
    checkmate::assertClass(parameters,"ParameterSet")
    private$.parameters <- parameters$clone()$update()
  }

  if(!is.null(paramValues)){
    checkmate::assertList(paramValues)
    self$setParameterValue(paramValues)
  }

  if(!is.null(decorators)){
    lapply(decorators,function(x){
      methods <- c(x$public_methods, get(paste0(x$inherit))$public_methods)
      methods <- methods[!(names(methods) %in% c("initialize","clone"))]

      for(i in 1:length(methods)){
          formals(methods[[i]]) = c(formals(methods[[i]]),list(self=self))
          assign(names(methods)[[i]],methods[[i]],envir=as.environment(self))
      }
    })
  }
  private$.decorators = unlist(lapply(decorators,function(x) x[["classname"]]))

  # Update skewness and kurtosis
  x = try(self$kurtosis(excess = TRUE), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$kurtosis <- NULL
  else
    private$.properties$kurtosis <- exkurtosisType(x)

  x = try(self$skewness(), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$skewness <- NULL
  else
    private$.properties$skewness <- skewType(x)

 # private$.setWorkingSupport()

  invisible(self)
}) # IN PROGRESS/NEEDS TESTING
Distribution$set("public","strprint",function(){
  if(length(private$.parameters)!=0){
    string = paste(apply(self$parameters(as.df = T)[self$parameters(as.df = T)$settable,],1,
                         function(x) paste(x[1],trimws(x[2]),sep=" = ")
                         ),
                   collapse=", ")
    string = paste0(self$short_name(),"(",string,")")
  } else {
    string = paste0(self$short_name())
  }
  return(string)
}) # DONE
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
}) # DONE
Distribution$set("public","summary",function(full=T){
  if(full){
    if(length(private$.parameters)!=0){
      cat(self$name(),"with parameterisation:\n")
      cat("\t",paste(self$parameters()["name"][[1]], self$parameters()["value"][[1]],
                     sep = " = ", collapse = "; "))
    } else
      cat(self$name(),"\n")
    cat("\n\n Quick Statistics: \n")

    if(!is.null(self$expectation())) cat("\tMean")
    if(!is.null(self$var())) cat("\tVariance")
    if(!is.null(self$getSkewness())) cat("\tSkewness")
    if(!is.null(self$getKurtosis())) cat("\tExcess Kurtosis")
    cat("\n")

    if(!is.null(self$expectation())) cat(self$expectation())
    if(!is.null(self$var())) cat(self$var())
    if(!is.null(self$getSkewness())) cat(self$getSkewness())
    if(!is.null(self$getKurtosis())) cat(self$getKurtosis())
    cat("\n")

    cat("Support:",self$support()$strprint())
    cat("\n Traits: ",self$valueSupport(),"; ",self$variateForm(),"\n\t See getTraits() for more",sep="")
    cat("\n Properties: ",self$getKurtosis(),"; ",self$getSkewness(),"; ",self$symmetry(),"\n\t See getProperties() for more",sep="")

    cat("\n\n Decorated with: ", unlist(private$.decorators))
  } else {
    if(length(private$.parameters)!=0){
      cat(self$short_name(),"distribution with parameterisation: ")
      cat(paste(self$parameters()["name"][[1]], self$parameters()["value"][[1]],
                sep = " = ", collapse = "; "))
    } else
        cat(self$name())
    cat("\n Scientific Type:",self$type()$strprint(),"\t See getTraits() for more")
    cat("\n Support:",self$support()$strprint(),"\t See getProperties() for more")
  }
}) # NEEDS TESTING
Distribution$set("public","plot",function(){}) # TO DO
Distribution$set("public","qqplot",function(){}) # TO DO

# Details Accessors
Distribution$set("public","name",function(){
  return(private$.name)
}) # NEEDS TESTING
Distribution$set("public","short_name",function(){
  return(private$.short_name)
}) # NEEDS TESTING
Distribution$set("public","description",function(){
  return(private$.description)
}) # NEEDS TESTING
Distribution$set("public","decorators",function(){
  return(private$.decorators)
}) # NEEDS TESTING

# Traits Accessors
Distribution$set("public","traits",function(){
  return(private$.traits)
}) # NEEDS TESTING
Distribution$set("public","valueSupport",function(){
  return(private$.traits[["valueSupport"]])
}) # NEEDS TESTING
Distribution$set("public","variateForm",function(){
  return(private$.traits[["variateForm"]])
}) # NEEDS TESTING
Distribution$set("public","type",function(){
  return(private$.traits[["type"]])
}) # NEEDS TESTING

# Properties Accessors
Distribution$set("public","properties",function(){
  return(private$.properties)
}) # NEEDS TESTING
Distribution$set("public","support",function(){
  return(private$.properties[["support"]])
}) # NEEDS TESTING
Distribution$set("public","distrDomain",function(){
  return(private$.properties[["distrDomain"]])
}) # NEEDS TESTING
Distribution$set("public","symmetry",function(){
  return(private$.properties[["symmetric"]])
}) # NEEDS TESTING

# Parameter Accessors
Distribution$set("public","parameters",function(id,as.df = F){
  return(private$.parameters$parameters(id, as.df))
}) # DONE
Distribution$set("public","getParameterValue",function(id){
  return(private$.parameters$getParameterValue(id))
}) # DONE
Distribution$set("public","setParameterValue",function(lst){

  self$parameters()$setParameterValue(lst)

  # Update skewness and kurtosis
  x = try(self$kurtosis(excess = TRUE), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$kurtosis <- NULL
  else
    private$.properties$kurtosis <- exkurtosisType(x)

  x = try(self$skewness(), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$skewness <- NULL
  else
    private$.properties$skewness <- skewType(x)


  #private$.setWorkingSupport()
  invisible(self)
}) # DONE

# Basic maths/stats
Distribution$set("public","pdf",function(x, log = FALSE){

  y = x

  y[!self$liesInSupport(x, F)] = 0

  if(log)
    y[self$liesInSupport(x, F)] = log(private$.pdf(x[self$liesInSupport(x, F)], self = self))
  else
    y[self$liesInSupport(x, F)] = private$.pdf(x[self$liesInSupport(x, F)], self = self)

  return(y)
}) # NEEDS TESTING
Distribution$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE){
  if(self$liesInSupport(q)){
      return(private$.cdf(q, lower.tail = FALSE, log.p = FALSE, self = self))
  }else
    warning(sprintf("%s does not lie in the support of %s",x,getR6Class(self)))
}) # NEEDS TESTING
Distribution$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  checkmate::assertNumeric(p, lower = 0, upper = 1)
  return(private$.quantile(p, lower.tail = TRUE, log.p = FALSE, self = self))
}) # NEEDS TESTING
Distribution$set("public","rand",function(n){
  return(private$.rand(n))
}) # NEEDS TESTING
Distribution$set("public","expectation",function(trafo){
  if(missing(trafo)){
    trafo = function(x) return(x)
  }
  if(testDiscrete(self)){
    rng = try(self$inf():self$sup(),silent = T)
    if(inherits(rng,"try-error"))
      rng = private$.getWorkingSupportRange()
    pdfs = self$pdf(rng)
    xs = trafo(rng)
    xs[pdfs==0] = 0
    return(sum(pdfs * xs))
  } else if(testContinuous(self)){
    message("Results from numerical integration are approximate only, better results may be available.")
    return(suppressMessages(integrate(function(x) {
      pdfs = self$pdf(x)
      xs = trafo(x)
      xs[pdfs==0] = 0
      return(xs * pdfs)
      }, lower = self$inf(), upper = self$sup())$value))
  }
}) # IN PROGRESS
Distribution$set("public","var",function(show.error = FALSE){
  return(self$expectation(trafo = function(x) x^2) - self$expectation()^2)
}) # IN PROGRESS
Distribution$set("public","sd",function(){
  return(sqrt(self$var()))
}) # DONE
Distribution$set("public","cov",function(){
  if(testUnivariate(self))
    return(self$var())
}) # TO DO
Distribution$set("public","cor",function(){}) # TO DO
Distribution$set("public","median",function(){
  self$quantile(0.5)
}) # DONE
Distribution$set("public","mode",function(which = 1){
  if(which==1){
    if(testDiscrete(self)){
      rng = try(self$inf():self$sup(),silent = T)
      if(inherits(rng,"try-error"))
        rng = self$getWorkingSupport()
      return(rng[which.max(self$pdf(rng))])
    } else if(testContinuous(self))
        return(optimize(self$pdf,c(self$inf(),1e08), maximum = TRUE))
  }
}) # IN PROGRESS
Distribution$set("public","sup",function(){
  return(self$support()$sup())
}) # DONE
Distribution$set("public","inf",function(){
  return(self$support()$inf())
}) # DONE

# Validation Checks
Distribution$set("public","liesInSupport",function(x, all = TRUE){
  if(all)
    return(all(x >= self$inf()) & all(x <= self$sup()))
  else
    return(x >= self$inf() & x <= self$sup())
}) # DONE
Distribution$set("public","liesInType",function(x){
  return(all(x >= self$type()$lower()) & all(x <= self$type()$upper))
}) # NEEDS TESTING
Distribution$set("public","liesInDistrDomain",function(x){
  return(all(x >= self$distrDomain()$lower()) & all(x <= self$distrDomain()$upper))
}) # NEEDS TESTING

#-------------------------------------------------------------
# Distribution Private Variables
#-------------------------------------------------------------
Distribution$set("private",".traits",list()) # DONE
Distribution$set("private",".properties",list()) # DONE
Distribution$set("private",".name",character(0)) # DONE
Distribution$set("private",".short_name",character(0)) # DONE
Distribution$set("private",".description",NULL) # DONE
Distribution$set("private",".parameters",data.frame()) # DONE
Distribution$set("private",".decorators",list()) # DONE
Distribution$set("private",".pdf",NULL) # DONE
Distribution$set("private",".cdf",NULL) # DONE
Distribution$set("private",".rand",NULL) # DONE
Distribution$set("private",".quantile",NULL) # DONE
Distribution$set("private",".workingSupport",NULL) # DONE
