library(distrMod)
library(R6)
library(checkmate)
library(data.table)
#devtools::install_github("RaphaelS1/R62S3")
library(R62S3)

#-------------------------------------------------------------
# Distribution Definition
#-------------------------------------------------------------

f_setParams <- function(x){
  assert(inherits(x,c("data.frame","data.table","list")))
  if(testList(x)){
    x <- data.frame(x,stringsAsFactors = FALSE)
    private$param.set <- rbind(private$param.set,x)
  }
  return(invisible(self))
}
f_getParams <- function(x) return(private$param.set)
f_getParamByName <- function(name){
  return(self$getParams()[self$getParams()[,1] %in% name,])
}
f_getParamValueByName <- function(name){
  return(self$getParamByName(name)["value"][[1]])
}
f_setParamByName <- function(name,value){
  param <- self$getParams()[self$getParams()[,1] %in% name,]
  if(param$Class=="numeric")
    assertNumeric(value,lower = param$Lower, upper = param$Upper)
  if(param$Class=="integer"){
    value = as.integer(value)
    assertInteger(value,lower = param$Lower, upper = param$Upper)
  }
  private$param.set[private$param.set[,1] %in% name,2] <- value
  return(invisible(self))
}
f_print <- function(...){
  cat(self$strprint())
  invisible(self)
}
f_strprint <- function(){
  string = paste(apply(self$getParams(),1,function(x) paste(x[1],trimws(x[2]),sep="=")),
                 collapse=", ")
  string = paste0(self$short.name,"(",string,")")
  return(string)
}
f_getSupport <- function(){
  return(private$support)
}
f_getType <- function(){
  return(self$getTraits()["Type"])
}
f_getDataType <- function(){
  return(self$getTraits()["DataType"])
}
f_getKurtosisType <- function(){
  Kurtosis = self$kurtosis()
  if(Kurtosis > 0)
    return("Leptokurtic")
  else if(Kurtosis == 0)
    return("Mesokurtic")
  else
    return("Platykurtic")
}
f_getSkew <- function(){
  Skew = self$skewness()
  if(Skew > 0)
    return("Positive Skew")
  else if(Skew == 0)
    return("Symmetric")
  else
    return("Negative Skew")
}
f_summary <- function(full=T){
  if(full){
    cat(self$name,"with parameterisation:\n")
    cat("\t",paste(B$getParams()["Long_Name"][[1]], B$getParams()["value"][[1]],
                   sep = " = ", collapse = "; "))
    cat("\n\n Quick Statistics: \n")
    cat("\tMean \tVar \tSkewness \tExcess Kurtosis \n")
    cat("  ",self$mean(), self$var(), self$skewness(), self$kurtosis(), "\n", sep = "\t")
    cat("\n Support:",self$getSupport())
    cat("\n Traits: ",self$getType()[[1]],"; ",self$getDataType()[[1]],"\n\t See getTraits() for more",sep="")
    cat("\n Properties: ",self$getKurtosis()[[1]],"; ",self$getSkew()[[1]],"\n\t See getProperties() for more",sep="")
  } else {
    cat(self$name,"distribution with parameterisation: ")
    cat(paste(self$getParams()["name"][[1]], self$getParams()["value"][[1]],
              sep = " = ", collapse = "; "))
    cat("\n Support:",self$getSupport())
    cat("\n Traits:",self$getType()[[1]],";",self$getDataType()[[1]],"\t\t See getTraits() for more")
    cat("\n Properties:",self$getKurtosis()[[1]],";",self$getSkew()[[1]],"\t See getProperties() for more")
  }
}
f_getProperties <- function() {
  return(private$properties)
}
f_getTraits <- function() {
  return(private$traits)
}
Distribution <- R6Class("Distribution",
                         public = list(getParams = f_getParams,
                                       name = character(),
                                       symmetry = list(),
                                       image = list(),
                                       getParamValueByName = f_getParamValueByName,
                                       getParamByName = f_getParamByName,
                                       setParamByName = f_setParamByName,
                                       L2Deriv = list(),
                                       FisherInfo = list(),
                                       print = f_print,
                                       strprint = f_strprint,
                                       short.name = character(),
                                       summary = f_summary,
                                       getSupport = f_getSupport,
                                       getType = f_getType,
                                       getDataType = f_getDataType,
                                       getKurtosis = f_getKurtosisType,
                                       getSkew = f_getSkew,
                                       getProperties = f_getProperties,
                                       getTraits = f_getTraits
                         ),
                         private = list(param.set = data.frame(),
                                        setParams = f_setParams,
                                        private.properties = list(),
                                        properties = list(),
                                        traits = list(),
                                        support = c(-Inf,Inf)
                         )
)

rm(list = ls()[grepl("f_",ls())])
R62S3(Distribution)