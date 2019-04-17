library(R6)

testDecoratorA <- R6Class("testDecoratorA")
testDecoratorA$set("public","printA",function() print("Hello World A"))
testDecoratorB <- R6Class("testDecoratorB")
testDecoratorB$set("public","printB",function() print("Hello World B"))
testClass <- R6Class("testClass", lock_objects = FALSE)
testClass$set("public","printClass",function() print("Hello World C"))
testClass$set("public","initialize",function(decorators=NULL){
  if(!is.null(decorators)){
    lapply(decorators,function(x){
      methods <- x$public_methods
      methods <- methods[!(names(methods) %in% c("initialize","clone"))]
      for(i in 1:length(methods))
        assign(names(methods)[[i]],methods[[i]],envir=as.environment(self))
    })
  }
  invisible(self)
})


testClass$new()$printClass()
testClass$new(list(testDecoratorA))$printA()
testClass$new(list(testDecoratorA))$printB()
testClass$new(list(testDecoratorB))$printB()
t = testClass$new(list(testDecoratorA,testDecoratorB))
class(t)
t
