#-------------------------------------------------------------
# BinomialDistribution R6Class Definition
#-------------------------------------------------------------
BinomialDistribution <- R6Class("BinomialDistribution",
                                inherit = Distribution
                                )

#-------------------------------------------------------------
# BinomialDistribution Private Methods
#-------------------------------------------------------------
BinomialDistribution$set("private",".traits",{
  Dictionary$new(list(Type = "Discrete", DataType = "Univariate"))})

#-------------------------------------------------------------
# BinomialDistribution Public Methods
#-------------------------------------------------------------
BinomialDistribution$set("public","initialize",function(size=NULL,prob=NULL){
  if(is.null(prob))
    value = 0.5
  else
    value = prob
  private$setParams(list(name="prob",value=value,Nuisance=FALSE,Fixed=FALSE,
                         Default=0.5,Class="numeric",Lower=0,Upper=1,Long_Name = "Probability of success"))
  if(is.null(size))
    value = 10
  else
    value = size
  private$setParams(list(name="size",value=value,Nuisance=FALSE,Fixed=TRUE,
                         Default=1,Class="integer",Lower=0,Upper=Inf,Long_Name = "Number of trials"))
  private$.privateproperties = Dictionary$new(list(withEvalAsVar = TRUE,
                                                   withMDE = TRUE,
                                                   withEvalL2derivDistr = TRUE,
                                                   withSim = FALSE,
                                                   withArith = FALSE,
                                                   logExact = TRUE,
                                                   lowerExact = TRUE))
  private$.properties = Dictionary$new(list(KurtosisType = self$getKurtosis(),
                                           Skew =  self$getSkew()))
  self$name = "Binomial Distribution"
  self$symmetry = Dictionary$new(list(Type = "Spherical",Center=0.5))
  private$.support = c(0,1)
  self$short.name = "Binomial"
  invisible(self)
})
BinomialDistribution$set("public","density",function(x){
  dbinom(x,self$getParamValueByName("size"),self$getParamValueByName("prob"))})
BinomialDistribution$set("public","rand",function(n) {rbinom(n,self$getParamValueByName("size"),
                                                             self$getParamValueByName("prob"))})
BinomialDistribution$set("public","distribution",function(q){pbinom(q,self$getParamValueByName("size"),
                                                                    self$getParamValueByName("prob"))})
BinomialDistribution$set("public","quantile",function(p){
  qbinom(p,self$getParamValueByName("size"),
         self$getParamValueByName("prob"))
})
BinomialDistribution$set("public","mean",function() {self$getParamValueByName("prob")*
    self$getParamValueByName("size")})
BinomialDistribution$set("public","variance",function() {self$getParamValueByName("prob")*
    self$getParamValueByName("size") *
    (1-self$getParamValueByName("prob"))})
BinomialDistribution$set("public","skewness",function() {(1-(2*self$getParamValueByName("prob")))/
    sqrt(self$getParamValueByName("prob") *
           self$getParamValueByName("size") *
           (1-self$getParamValueByName("prob")))})
BinomialDistribution$set("public","kurtosis",function() {
  (1 - (6 * self$getParamValueByName("prob") *
          (1-self$getParamValueByName("prob"))))/
    (self$getParamValueByName("prob")*self$getParamValueByName("size")*
       (1-self$getParamValueByName("prob")))
  })

#-------------------------------------------------------------
# BinomialDistribution Private Variables
#-------------------------------------------------------------

#-------------------------------------------------------------
# BinomialDistribution Public Variables
#-------------------------------------------------------------
