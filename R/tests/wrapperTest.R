library(R6)

testClassA <- R6Class("testClassA")
testClassA$set("public","getSupport",function() return(private$support))
testClassA$set("private","support",NULL)
testClassA$set("public","initialize",function(x){
  private$support <- x
})
testClassA$set("public","anOldFunction",function(x){
  print("Old Function")
})

truncateWrapper <- R6Class("truncateWrapper")
truncateWrapper$set("private","origClass",NULL)
truncateWrapper$set("public","getOrigClass",function() return(private$origClass))
truncateWrapper$set("private","support",NULL)
truncateWrapper$set("public","getSupport",function() return(private$support))
truncateWrapper$set("public","initialize",function(x,lower,upper){
  private$origClass <- x
  oldsupport = x$getSupport()
  newsupport = oldsupport[oldsupport>lower & oldsupport<upper]
  private$support <- newsupport
})


t = testClassA$new(1:10)
t$getSupport()
t2 = truncateWrapper$new(t,3,6)
t2$getOrigClass()
t2$getSupport()

t2$getOrigClass()$anOldFunction()
t2$anOldFunction()

'$.R6' <- function(x,call,...){
  x[[".__enclos_env__"]][["private"]][["origClass"]][[call]](...)
}

t2$anOldFunction()
