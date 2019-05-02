#-------------------------------------------------------------
# Distribution R6Class Definition
#-------------------------------------------------------------
Distribution <- R6::R6Class("Distribution", lock_objects = FALSE)

#-------------------------------------------------------------
# Distribution Private Methods
#-------------------------------------------------------------
Distribution$set("private",".addParameter",
                 function(id, name, default, settable, fittable, class,
                          lower, upper, description, paramlist = NULL,
                          paramSet = NULL){
  if(!is.null(paramSet)){
    if(is(paramSet,"ParameterSet")){
      private$.parameters <- data.frame(private$.parameters, paramSet,
                                        stringsAsFactors = FALSE)
      class(private$.parameters) <- append(class(private$.parameters),"ParameterSet")
      invisible(self)
    } else
      stop("paramSet must be of class 'ParameterSet'")
  }
  if(!is.null(paramlist)){
    id = paramlist$id
    name = paramlist$name
    if(!is.null(paramlist$value))
      value = paramlist$value
    else
      value = paramlist$default
    default = paramlist$default
    settable = paramlist$settable
    fittable = paramlist$fittable
    class = paramlist$class
    lower = paramlist$lower
    upper = paramlist$upper
    description = paramlist$description
  }

  checkmate::assertCharacter(c(id, name, class, description),
    .var.name = "'id', 'name', 'class' and 'description' must be
                  characters")
  checkmate::assertNumeric(c(default, value, lower, upper))
  checkmate::assertLogical(c(settable, fittable))
  checkmate::assert(class == "numeric", class == "integer",
    .var.name = "'class' must be one of: 'numeric' or 'integer'")
  checkmate::assert(!(id %in% private$.parameters$id), .var.name = "parameter IDs must be unique")
  checkmate::assert(!(name %in% private$.parameters$name), .var.name = "parameter names must be unique")

  private$.parameters <- rbind(private$.parameters,
                               data.frame(id = id, name = name, value = value,
                                    default = default, settable = settable,
                                    fittable = fittable, class = class,
                                    lower = lower, upper = upper,
                                    description = description,
                                    stringsAsFactors = FALSE))

  if(!("ParameterSet" %in% class(private$.parameters)))
    class(private$.parameters) <- append(class(private$.parameters),"ParameterSet")
  invisible(self)
}) # NEEDS TESTING
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
                      type = reals$new(), support, distrDomain,
                      symmetric = logical(0),
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters, paramvalues,
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

  checkmate::assert(inherits(type,"sets"), .var.name = "type should be class 'sets'.")
  checkmate::assert(inherits(support,"sets"), inherits(distrDomain,"sets"),
                    .var.name = "'support' and 'distrDomain' should be class 'sets'.")

  if(!is.null(valueSupport))
    checkmate::assert(valueSupport == "continuous", valueSupport == "discrete",
           valueSupport == "mixture",
           .var.name = "valueSupport should be one of: 'continuous', 'discrete',
           'mixture'.")
  else if(class(support)[[1]] %in% c("reals","posReals","negReals"))
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
    if(is(parameters,"ParameterSet")){
      private$.parameters <- parameters
    } else {
      checkmate::assertList(parameters)
      lapply(parameters, function(x) {private$.addParameter(paramlist = x)})
    }
  }

  if(!missing(paramvalues)){
    checkmate::assertList(paramvalues)
    self$setParameterValue(paramvalues)
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
    string = paste(apply(self$parameters(),1,function(x) paste(x[1],trimws(x[3]),sep=" = ")),
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
Distribution$set("public","parameters",function(id){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(!missing(id)){
    id0 = id
    if(length(dplyr::filter(private$.parameters, id == id0))==0)
      return(private$.parameters)
    return(dplyr::filter(private$.parameters, id == id0))
  } else
    return(private$.parameters)
}) # NEEDS TESTING
Distribution$set("public","getParameterValue",function(id){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  val = self$parameters(id = id)[["value"]]
  if(length(val)==0)
    return(paste(id, "is not a parameter in this distribution."))
  else
    return(val[[1]])
}) # NEEDS TESTING
Distribution$set("public","setParameterValue",function(lst){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")
  checkmate::assertList(lst)

  for(i in 1:length(lst)){
    id <- names(lst)[[i]]
    value <- lst[[i]]

    param <- self$parameters()[self$parameters()[,"id"] %in% id,]
    if(length(param)==0)
      stop(sprintf("%s is not in the parameter set.",id))

    if(!param$settable)
      stop(sprintf("%s is not settable.",param$name))

    if(param$class=="numeric")
      checkmate::assertNumeric(value,lower = param$lower, upper = param$upper)
    if(param$class=="integer"){
      value = as.integer(value)
      checkmate::assertInteger(value,lower = param$lower, upper = param$upper)
    }
    private$.parameters[private$.parameters[,"id"] %in% param$id, "value"] <- value
  }

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
}) # NEEDS TESTING

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
    warning("Results from numerical integration are approximate only, better results may be available.")
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
