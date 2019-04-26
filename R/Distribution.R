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
                          lower, upper, description, paramlist = NULL){
  if(!is.null(paramlist)){
    id = paramlist$id
    name = paramlist$name
    default = paramlist$value
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
  checkmate::assertNumeric(c(default, lower, upper))
  checkmate::assertLogical(c(settable, fittable))
  checkmate::assert(class == "numeric", class == "integer",
    .var.name = "'class' must be one of: 'numeric' or 'integer'")
  checkmate::assert(!(id %in% private$.parameters$id), .var.name = "'id' must be unique")
  checkmate::assert(!(name %in% private$.parameters$name), .var.name = "'name' must be unique")

  private$.parameters <- rbind(private$.parameters,
                               data.frame(id = id, name = name, value = default,
                                    default = default, settable = settable,
                                    fittable = fittable, class = class,
                                    lower = lower, upper = upper,
                                    description = description)
  )
}) # NEEDS TESTING
#-------------------------------------------------------------
# Distribution Public Methods
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name, short_name,
                      type = reals$new(), support, distrDomain, symmetric,
                      pdf, cdf, quantile, rand, parameters, decorators = NULL,
                      valueSupport = NULL, variateForm = NULL, description=NULL
                      ){

  # Validation checks
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

  if(!missing(pdf))
    private$.pdf <- pdf
  else
    private$.pdf <- function(...){
      warning("Density/mass function is missing.")
      return(NULL)
    }

  if(!missing(cdf))
    private$.cdf <- cdf
  else
    private$.cdf <- function(...){
      warning("Distribution function is missing.")
      return(NULL)
    }

  if(!missing(quantile))
    private$.quantile <- quantile
  else
    private$.quantile <- function(...){
      warning("Quantile function is missing.")
      return(NULL)
    }

  if(!missing(rand))
    private$.rand <- rand
  else
    private$.rand <- function(...){
      warning("Random generation function is missing.")
      return(NULL)
    }


  if(!missing(parameters)){
    checkmate::assertList(parameters)
    lapply(parameters, function(x) private$.addParameter(x))
  }

  if(!is.null(decorators)){
    lapply(decorators,function(x){
      methods <- x$public_methods
      methods <- methods[!(names(methods) %in% c("initialize","clone"))]
      for(i in 1:length(methods))
        assign(names(methods)[[i]],methods[[i]],envir=as.environment(self))
    })
  }
  private$.decorators = unlist(lapply(decorators,function(x) x[["classname"]]))

  invisible(self)
}) # IN PROGRESS/NEEDS TESTING
Distribution$set("public","strprint",function(){
  if(length(private$.parameters)!=0){
    string = paste(apply(self$getParameters(),1,function(x) paste(x[1],trimws(x[2]),sep="=")),
                   collapse=", ")
    string = paste0(self$short_name(),"(",string,")")
  } else {
    string = paste0(self$short_name())
  }
  return(string)
}) # NEEDS TESTING
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
}) # NEEDS TESTING
Distribution$set("public","summary",function(full=T){
  if(full){
    if(length(private$.parameters)!=0){
      cat(self$name(),"with parameterisation:\n")
      cat("\t",paste(self$parameters()["Long_Name"][[1]], self$parameters()["value"][[1]],
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
      cat(self$short_name()," distribution with parameterisation: ")
      cat(paste(self$parameters()["name"][[1]], self$parameters()["value"][[1]],
                sep = " = ", collapse = "; "))
    } else
        cat(self$name())
    cat("\n Support:",self$support()$strprint())
    cat("\n Traits:",self$type()$strprint(),"\t\t See getTraits() for more")
    cat("\n Properties:",self$getKurtosis(),";",self$getSkewness(),"\t See getProperties() for more")
  }
}) # IN PROGRESS
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
  return(private$.properties[["symmetry"]])
}) # NEEDS TESTING
Distribution$set("public","getSkewness",function(){
  return(private$.properties[["skewness"]])
}) # NEEDS TESTING
Distribution$set("public","getKurtosis",function(){
  return(private$.properties[["kurtosis"]])
}) # NEEDS TESTING

# Parameter Accessors
Distribution$set("public","parameters",function(id, name){
  if(length(private$.parameters)==0)
    return("There are no parameters in this distribution.")

  if(!missing(id))
    return(private$.parameters[id])
  else if(!missing(name))
    return(private$.parameters[name])
  else
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

  if(!missing(id))
    param <- self$parameters()[self$parameters()[,1] %in% id,]
  else if(!missing(name))
    param <- self$parameters()[self$parameters()[,1] %in% name,]
  else
    stop("One of 'id' or 'name' must be given.")

  if(!param$settable)
    stop(sprintf("%s is not settable.",param$name))

  if(param$class=="numeric")
    checkmate::assertNumeric(value,lower = param$Lower, upper = param$Upper)
  if(param$class=="integer"){
    value = as.integer(value)
    checkmate::assertInteger(value,lower = param$Lower, upper = param$Upper)
  }
  private$.parameters[private$.parameters[,"id"] %in% param$id, "value"] <- value
  return(invisible(self))
}) # NEEDS TESTING

# Basic maths/stats
Distribution$set("public","pdf",function(x, log = FALSE){
  return(private$.pdf(x, log = FALSE))
})
Distribution$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE){
  return(private$.cdf(q, lower.tail = TRUE, log.p = FALSE))
})
Distribution$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  return(private$.quantile(p, lower.tail = TRUE, log.p = FALSE))
})
Distribution$set("public","rand",function(n){
  return(private$.rand(n))
})
Distribution$set("public","expectation",function(){}) # TO DO
Distribution$set("public","var",function(){}) # TO DO
Distribution$set("public","sd",function(){
  return(sqrt(self$var))
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
}) # TO DO
Distribution$set("public","sup",function(){
  return(self$support()$sup())
}) # NEEDS TESTING
Distribution$set("public","inf",function(){
  return(self$support()$inf())
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