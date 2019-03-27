library(distrMod)
library(R6)
library(checkmate)
library(data.table)

#-------------------------------------------------------------
# Current Method with distrMod to call and use binomial family
#-------------------------------------------------------------

bin <- BinomFamily()

#-------------------------------------------------------------
# Example of R6 upgrade for distributions. Note that this does not
#   include full abstraction. Additionally code is not optimal/efficient,
#   this should be viewed as a rough guide for how we would remove classes
#   and convert them to lists as well as how to make use of inheritance.
#-------------------------------------------------------------
setParams <- function(x){
  assert(inherits(x,c("data.frame","data.table","list")))
  if(testList(x)){
    x <- data.frame(x,stringsAsFactors = FALSE)
    private$param.set <- rbind(private$param.set,x)
  }

}
getParams <- function(x) return(private$param.set)
getParamByName <- function(name){
  return(self$getParams()[self$getParams()[,1] %in% name,2])
}
setParamByName <- function(name,value){
  param <- self$getParams()[self$getParams()[,1] %in% name,]
  if(param$Class=="numeric")
    assertNumeric(value,lower = param$Lower, upper = param$Upper)
  if(param$Class=="integer"){
    value = as.integer(value)
    assertInteger(value,lower = param$Lower, upper = param$Upper)
  }
  private$param.set[private$param.set[,1] %in% name,2] <- value
}
print <- function(...){
  cat(self$strprint())
  invisible(self)
}
strprint <- function(){
  string = paste(apply(self$getParams(),1,function(x) paste(x[1],trimws(x[2]),sep="=")),
                 collapse=", ")
  string = paste0(self$short.name,"(",string,")")
  return(string)
}
distribution6 <- R6Class("R6 Distributions",
                         public = list(getParams = getParams,
                                       properties = list(),
                                       name = character(),
                                       symmetry = list(),
                                       support = c(-Inf,Inf),
                                       image = list(),
                                       getParamByName = getParamByName,
                                       setParamByName = setParamByName,
                                       L2Deriv = list(),
                                       FisherInfo = list(),
                                       print = print,
                                       strprint = strprint,
                                       short.name = character()
                                       ),
                         private = list(param.set = data.frame(),
                                        setParams = setParams,
                                        private.properties = list()
                                        )
                         )
rm(getParamByName,getParams,setParamByName,setParams,print)
binomial6 <- R6Class("binomial6",
                     inherit = distribution6,
                     private = list(
                     ),
                     public = list(
                       initialize = function(size=NULL,prob=NULL){
                         if(is.null(prob))
                           value = 0.5
                         else
                           value = prob
                         private$setParams(list(name="prob",value=value,Nuisance=FALSE,Fixed=FALSE,
                                          Default=0.5,Class="numeric",Lower=0,Upper=1))
                         if(is.null(size))
                           value = 1
                         else
                           value = size
                         private$setParams(list(name="size",value=value,Nuisance=FALSE,Fixed=TRUE,
                                          Default=1,Class="integer",Lower=0,Upper=Inf))
                         private$private.properties = list(withEvalAsVar = TRUE,
                                                           withMDE = TRUE,
                                                           withEvalL2derivDistr = TRUE,
                                                           withSim = FALSE,
                                                           withArith = FALSE,
                                                           logExact = TRUE,
                                                           lowerExact = TRUE)
                         self$properties = list()
                         self$name = "Binomial Distribution"
                         self$symmetry = list(Type = "Spherical",Center=0.5)
                         self$support = c(0,1)
                         self$image = list(space="Naturals",dim=1)
                         self$L2Deriv = list(Class="EuclRandVarList",
                                             Domain = "Reals.D1",
                                             Symm.type = "Odd",
                                             Symm.center = 0.5,
                                             Distr = "AffLinLatticeDistribution",
                                             DistrSymm.Type = "Spherical",
                                             DistrSymm.Center = 0,
                                             fct = function (param)
                                             {
                                               prob <- main(param)
                                               fct <- function(x) {
                                               }
                                               body(fct) <- substitute({
                                                 (x - size * prob)/(prob * (1 - prob))
                                               }, list(size = size, prob = prob))
                                               return(fct)
                                             })
                         self$FisherInfo = list(Class = "PosSemDefSymmMatrix",
                                                prob = 4,
                                                fct = function (param)
                                                {
                                                  prob <- main(param)
                                                  PosDefSymmMatrix(matrix(size/(prob * (1 - prob)), dimnames = list("prob",
                                                                                                                    "prob")))
                                                })
                         self$short.name = "Binomial"
                         },
                       r = function(n) rbinom(n,self$getParamByName("size"),
                                              self$getParamByName("prob")),
                       d = function(x) dbinom(x,self$getParamByName("size"),
                                              self$getParamByName("prob")),
                       p = function(q) pbinom(q,self$getParamByName("size"),
                                              self$getParamByName("prob")),
                       q = function(p) qbinom(p,self$getParamByName("size"),
                                              self$getParamByName("prob"))
                       ))
strprint.binomial6 <- function(x,...){
  x$strprint()
}
strprint.list <- function(x,...){
  lapply(x,strprint,...)
}
B <- binomial6$new(prob=0.6,size=10)
B$getParams()
B$r(10)
B$d(10)
B$q(0.3)
B$p(1)
B$getParamByName("size")
B$L2Deriv
B$setParamByName("prob",0.2)
B$getParamByName("prob")

# And an example of a way to list all implemented distributions
listDistributions <- function(env=TRUE,dist=TRUE){
  y = sapply(ls(name=".GlobalEnv"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "R6 Distributions_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  if(env & dist)
    return(y[y!="FALSE"])
  else if(env)
    return(names(y[y!="FALSE"]))
  else if(dist)
    return(as.character(y[y!="FALSE"]))
}
listDistributions() # Returns environment and distribution names
listDistributions(dist=FALSE) # Returns environment name only
listDistributions(env=FALSE) # Returns distribution name only
