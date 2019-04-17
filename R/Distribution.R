library(R6)
library(checkmate)
library(data.table)
#devtools::install_github("RaphaelS1/R62S3")
library(R62S3)
#devtools::install_github("RaphaelS1/dictionaryR6")
library(dictionaryR6)

#-------------------------------------------------------------
# Distribution R6Class Definition
#-------------------------------------------------------------
Distribution <- R6Class("Distribution")

#-------------------------------------------------------------
# Distribution Private Methods
#-------------------------------------------------------------
Distribution$set("private","setParams",function(x){
  assert(inherits(x,c("data.frame","data.table","list")))
  if(testList(x)){
    x <- data.frame(x,stringsAsFactors = FALSE)
    private$.param.set <- rbind(private$.param.set,x)
  }
  invisible(self)
})

#-------------------------------------------------------------
# Distribution Public Methods
#-------------------------------------------------------------
Distribution$set("public","initialize",function(){
  stop("Distribution is an abstract class that can't be initialized.")
})
Distribution$set("public","getParams",function(x){
  return(private$.param.set)})
Distribution$set("public","getParamByName",function(name){
  return(self$getParams()[self$getParams()[,1] %in% name,])
})
Distribution$set("public","getParamValueByName",function(name){
  return(self$getParamByName(name)["value"][[1]])
})
Distribution$set("public","setParamByName",function(name,value){
  param <- self$getParams()[self$getParams()[,1] %in% name,]
  if(param$Class=="numeric")
    assertNumeric(value,lower = param$Lower, upper = param$Upper)
  if(param$Class=="integer"){
    value = as.integer(value)
    assertInteger(value,lower = param$Lower, upper = param$Upper)
  }
  private$.param.set[private$.param.set[,1] %in% name,2] <- value
  return(invisible(self))
})
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
})
Distribution$set("public","strprint",function(){
  string = paste(apply(self$getParams(),1,function(x) paste(x[1],trimws(x[2]),sep="=")),
                 collapse=", ")
  string = paste0(self$short.name,"(",string,")")
  return(string)
})
Distribution$set("public","getSupport",function(){
  return(private$.support)
})
Distribution$set("public","getType",function(){
  return(self$getTraits()$get("Type"))
})
Distribution$set("public","getDataType",function(){
  return(self$getTraits()$get("DataType"))
})
Distribution$set("public","getKurtosis",function(){
  Kurtosis = self$kurtosis()
  if(Kurtosis > 0)
    return("Leptokurtic")
  else if(Kurtosis == 0)
    return("Mesokurtic")
  else
    return("Platykurtic")
})
Distribution$set("public","getSkew",function(){
  Skew = self$skewness()
  if(Skew > 0)
    return("Positive Skew")
  else if(Skew == 0)
    return("Symmetric")
  else
    return("Negative Skew")
})
Distribution$set("public","summary",function(full=T){
  if(full){
    cat(self$name,"with parameterisation:\n")
    cat("\t",paste(self$getParams()["Long_Name"][[1]], self$getParams()["value"][[1]],
                   sep = " = ", collapse = "; "))
    cat("\n\n Quick Statistics: \n")
    cat("\tMean \tVar \tSkewness \tExcess Kurtosis \n")
    cat("  ",self$mean(), self$variance(), self$skewness(), " ", self$kurtosis(), "\n", sep = "\t")
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
})
Distribution$set("public","getProperties",function() {
  return(private$.properties)
})
Distribution$set("public","getTraits",function() {
  return(private$.traits)
})

#-------------------------------------------------------------
# Distribution Private Variables
#-------------------------------------------------------------
Distribution$set("private",".param.set",data.frame())
Distribution$set("private",".privateproperties",Dictionary$new())
Distribution$set("private",".properties",Dictionary$new())
Distribution$set("private",".traits",Dictionary$new())
Distribution$set("private",".support",c(-Inf,Inf))

#-------------------------------------------------------------
# Distribution Public Variables
#-------------------------------------------------------------
Distribution$set("public","name",character())
Distribution$set("public","short.name",character())
Distribution$set("public","symmetry",Dictionary$new())
Distribution$set("public","image",Dictionary$new())
