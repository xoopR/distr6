f_r = function(n) {rbinom(n,self$getParamValueByName("size"),
                       self$getParamValueByName("prob"))}
f_d = function(x) {dbinom(x,self$getParamValueByName("size"),
                         self$getParamValueByName("prob"))}
f_p = function(q) {pbinom(q,self$getParamValueByName("size"),
                         self$getParamValueByName("prob"))}
f_q = function(p){
  qbinom(p,self$getParamValueByName("size"),
                         self$getParamValueByName("prob"))
}
f_mean = function() {self$getParamValueByName("prob")*
    self$getParamValueByName("size")}
f_var = function() {self$getParamValueByName("prob")*
    self$getParamValueByName("size") *
    (1-self$getParamValueByName("prob"))}
f_skewness = function() {(1-(2*self$getParamValueByName("prob")))/
    sqrt(self$getParamValueByName("prob") *
           self$getParamValueByName("size") *
           (1-self$getParamValueByName("prob")))}
f_kurtosis = function() {(1 - (6 * self$getParamValueByName("prob") *
                                 (1-self$getParamValueByName("prob"))))/
    (self$getParamValueByName("prob")*self$getParamValueByName("size")*
       (1-self$getParamValueByName("prob")))}


BinomialDistribution <- R6Class("BinomialDistribution",
                     inherit = Distribution,
                     private = list(
                       traits = list(Type = "Discrete", DataType = "Univariate")
                     ),
                     public = list(
                       initialize = function(size=NULL,prob=NULL){
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
                         private$private.properties = list(withEvalAsVar = TRUE,
                                                           withMDE = TRUE,
                                                           withEvalL2derivDistr = TRUE,
                                                           withSim = FALSE,
                                                           withArith = FALSE,
                                                           logExact = TRUE,
                                                           lowerExact = TRUE)
                         private$properties = list(KurtosisType = self$getKurtosis(),
                                                   Skew =  self$getSkew())
                         self$name = "Binomial Distribution"
                         self$symmetry = list(Type = "Spherical",Center=0.5)
                         private$support = c(0,1)
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
                       r = f_r,
                       d = f_d,
                       p = f_p,
                       q = f_q,
                       mean = f_mean,
                       var = f_var,
                       skewness = f_skewness,
                       kurtosis = f_kurtosis
                     ))

rm(list = ls()[grepl("f_",ls())])
R62S3(BinomialDistribution)