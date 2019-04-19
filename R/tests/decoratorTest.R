library(R6)

decorator <- R6Class("decorator")
decorator$set("public","initialize",function(testclass){
  decorators = testclass$decorators
  if(!is.null(decorators)){
    decorators = lapply(decorators,get)
  }
  decorators = unique(c(decorators,get(class(self)[[1]])))

  assign(paste0(substitute(testclass)),testClass$new(decorators),pos=.GlobalEnv)

  cat(paste(substitute(testclass),"is now decorated with",
            get(class(self)[[1]])$classname,"\n"))
})

testDecoratorA <- R6Class("testDecoratorA", inherit = decorator)
testDecoratorA$set("public","printA",function() print("Hello World A"))

testDecoratorB <- R6Class("testDecoratorB", inherit = decorator)
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
  self$decorators = unlist(lapply(decorators,function(x) x[["classname"]]))
  invisible(self)
})
testClass$set("public","decorators",list())

testClass$new()$printClass()
x = testClass$new(list(testDecoratorB))
testDecoratorA$new(x)
x$decorators
