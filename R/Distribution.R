#install.packages("checkmate"); install.packages("R6");
library(R6)

#-------------------------------------------------------------
# Distribution R6Class Definition
#-------------------------------------------------------------
Distribution <- R6Class("Distribution", lock_objects = FALSE)

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
  checkmate::assert(!(id %in% private$.parameters$id), .var.name = "'id' must be unique")
  checkmate::assert(!(name %in% private$.parameters$name), .var.name = "'name' must be unique")

  private$.parameters <- rbind(private$.parameters,
                               data.frame(id = id, name = name, value = value,
                                    default = default, settable = settable,
                                    fittable = fittable, class = class,
                                    lower = lower, upper = upper,
                                    description = description,
                                    stringsAsFactors = FALSE))
  class(private$.parameters) <- append(class(private$.parameters),"ParameterSet")
  invisible(self)
}) # NEEDS TESTING
#-------------------------------------------------------------
# Distribution Public Methods
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name, short_name,
                      type = reals$new(), support = reals$new(),
                      distrDomain = reals$new(), symmetric = logical(0),
                      pdf, cdf, quantile, rand, parameters, paramvalues,
                      decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL
                      ){

  # Validation checks
  if(missing(short_name)) short_name = name
  checkmate::assertCharacter(c(name, short_name),
                             .var.name = "'name' and 'short_name' must be of class 'character'.")
  checkmate::assert(inherits(type,"sets"), .var.name = "type should be class 'sets'.")
  checkmate::assert(inherits(support,"sets"), inherits(distrDomain,"sets"),
                    .var.name = "'support' and 'distrDomain' should be class 'sets'.")
  checkmate::assertLogical(symmetric)

  private$.name <- name
  private$.short_name <- short_name
  if(!is.null(description))
    private$.description <- description

  if(!is.null(valueSupport))
    checkmate::assert(valueSupport == "continuous", valueSupport == "discrete",
           valueSupport == "mixture",
           .var.name = "valueSupport should be one of: 'continuous', 'discrete',
           'mixture'.")
  else if(class(type)[[1]] %in% c("reals","posReals","negReals"))
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

  if(!missing(pdf)){
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

  if(!missing(cdf)){
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

  if(!missing(quantile)){
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

  if(!missing(rand)){
    if(!is.null(formals(rand)$self))
      formals(rand)$self = self
    else
      formals(rand) = c(formals(rand),list(self=self))
    private$.rand <- rand
  } else
    private$.rand <- function(...){
      warning("Random generation function is missing.")
      return(NULL)
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
          formals(methods[[i]] ) = c(formals(methods[[i]]),list(self=self))
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
Distribution$set("public","parameters",function(id, name){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(!missing(id)){
    id0 = id
    return(dplyr::filter(private$.parameters, id == id0))
  } else if(!missing(name)) {
    name0 = id
    return(dplyr::filter(private$.parameters, name == name0))
  } else
    return(private$.parameters)
}) # NEEDS TESTING
Distribution$set("public","getParameterValue",function(id, name){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(!missing(id))
    return(self$parameters(id = id)["value"][[1]])
  else if(!missing(name))
    return(self$parameters(name = name)["value"][[1]])
  else
    stop("One of 'id' or 'name' must be given.")
}) # NEEDS TESTING
Distribution$set("public","setParameterValue",function(value, id, name){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(checkmate::testList(value)){
    lst <- value
    for(i in 1:length(lst)){
      id <- names(lst)[[i]]
      value <- lst[[i]]

      if(!missing(id))
        param <- self$parameters()[self$parameters()[,1] %in% id,]
      else if(!missing(name))
        param <- self$parameters()[self$parameters()[,1] %in% name,]
      else
        stop("One of 'id' or 'name' must be given.")

      if(!param$settable)
        stop(sprintf("%s is not settable.",param$name))

      if(param$class=="numeric")
        checkmate::assertNumeric(value,lower = param$lower, upper = param$upper)
      if(param$class=="integer"){
        value = as.integer(value)
        checkmate::assertInteger(value,lower = param$lower, upper = param$upper)
      }
      private$.parameters[private$.parameters[,"id"] %in% param$id, "value"] <- value
      invisible(self)
    }
  } else {

    if(!missing(id))
      param <- self$parameters()[self$parameters()[,1] %in% id,]
    else if(!missing(name))
      param <- self$parameters()[self$parameters()[,1] %in% name,]
    else
      stop("One of 'id' or 'name' must be given.")

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
  else{
    private$.properties$skewness <- skewType(x)

    invisible(self)
  }
}) # NEEDS TESTING

# Basic maths/stats
Distribution$set("public","pdf",function(x, log = FALSE){
  if(self$liesInSupport(x)){
    if(log)
      return(log(private$.pdf(x, self = self)))
    else
      return(private$.pdf(x, self = self))
  }else
    return(0)
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
  return(private$.rand(n, self = self))
}) # NEEDS TESTING
Distribution$set("public","expectation",function(trafo){
  if(missing(trafo)){
    trafo = function(x) return(x)
  }
  if(testDiscrete(self)){
    pdfs = self$pdf(self$support()$numeric())
    xs = trafo(self$support()$numeric())
    xs[pdfs==0] = 0
    return(sum(pdfs * xs))
  } else if(testContinuous(self)){
    return(integrate(function(x) {
      pdfs = self$pdf(x)
      xs = trafo(x)
      xs[pdfs==0] = 0
      return(xs * pdfs)
      }, lower = self$inf(), upper = self$sup())$value)
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
    if(testDiscrete(self))
      return(self$support()$numeric()[which.max(self$pdf(self$support()$numeric()))])
    else if(testContinuous(self))
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
Distribution$set("public","liesInSupport",function(x){
  return(all(x >= self$inf()) & all(x <= self$sup()))
}) # DONE
Distribution$set("public","liesInType",function(x){
  return(all(x >= self$type()$lower()) & all(x <= self$type()$upper))
}) # NEEDS TESTING
Distribution$set("public","liesInDistrDomain",function(x){
  return(all(x >= self$distrDomain()$lower()) & all(x <= self$distrDomain()$upper))
}) # NEEDS TESTING

# Mathematical Operations
Distribution$set("public","convolution",function(distribution, add = TRUE){
  assertDistribution(distribution)
  subpdf1 <- self$.__enclos_env__$private$.pdf
  subpdf2 <- distribution$.__enclos_env__$private$.pdf
  sublower <- distribution$inf()

  if(testContinuous(distribution)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
          return(sapply(x,function(z){
            integrate(f = function(y){pdf1(z - y)*pdf2(y)},
                      lower = alower, upper = z)$value
            }))
      },list(pdf1 = subpdf1, pdf2 = subpdf2, alower = sublower))
    } else {
      body(fnc) <- substitute({
        return(sapply(x,function(z){
            integrate(f = function(y){pdf1(y - z)*pdf2(y)},
                      lower = alower, upper = z)$value
        }))
      },list(pdf1 = subpdf1, pdf2 = subpdf2, alower = sublower))
    }
  } else if(testDiscrete(distribution)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(alower, z, by = 1)
          sum(pdf1(z - support) * pdf2(support))
        }))
      },list(pdf1 = subpdf1, pdf2 = subpdf2, alower = sublower))
    } else {
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(alower, z, by = 1)
            sum(pdf1(support - z) * pdf2(support))
          }))
        },list(pdf1 = subpdf1, pdf2 = subpdf2, alower = sublower))
    }
  }
    return(fnc)
})

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