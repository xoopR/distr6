# 'Adapter' test
testClassA <- R6Class("testClassA")
testClassA$set("public","printerA",function() print("Hello A World"))
testClassB <- R6Class("testClassB")
testClassB$set("public","printerB",function() print("Hello B World"))
testAdaptor <- R6Class("testAdaptor")
testAdaptor$set("public","printer",function(){
  if(self$type=="A") testClassA$new()$printerA()
  else testClassB$new()$printerB()
})
testAdaptor$set("public","type",character(0))
testAdaptor$set("public","initialize",function(x) self$type = x)

t = testAdaptor$new("B")
t$printer()
t = testAdaptor$new("A")
t$printer()

# 'Multiple inheritance' test

Continuous <- R6Class("Continuous")
Continuous$set("public","publicSupport","Continuous")
Continuous$set("public","printPublicSupport",function() print("I am Continuous"))
Continuous$set("private","privateSupport", "Continuous")
Continuous$set("private","printPrivateSupport",function() print("I am Continuous"))

Univariate <- R6Class("Univariate")
Univariate$set("public","publicForm","Univariate")
Univariate$set("public","printPublicForm",function() print("I am Univariate"))
Univariate$set("private","privateForm", "Univariate")
Univariate$set("private","printPrivateForm",function() print("I am Univariate"))

testMultiple <- R6Class("testMultiple", lock_objects=F,private=list(.Type="XvT"))
testMultiple$set("public","initialize",function(VariateForm, ValueSupport){

  # Public Methods & Variables
  methods <- c(VariateForm[["public_methods"]],VariateForm[["public_fields"]])
  methods <- methods[!(names(methods) %in% c("initialize","clone"))]
    for(i in 1:length(methods))
      assign(names(methods)[[i]],methods[[i]],envir=as.environment(self))

  methods <- c(ValueSupport[["public_methods"]],ValueSupport[["public_fields"]])
  methods <- methods[!(names(methods) %in% c("initialize","clone"))]
    for(i in 1:length(methods))
      assign(names(methods)[[i]],methods[[i]],envir=as.environment(self))

  # Private Methods & Variables
  methods <- c(VariateForm[["private_methods"]],VariateForm[["private_fields"]])
  methods <- methods[!(names(methods) %in% c("initialize","clone"))]
  for(i in 1:length(methods))
    assign(names(methods)[[i]],methods[[i]],envir=as.environment(private))

  methods <- c(ValueSupport[["private_methods"]],ValueSupport[["private_fields"]])
  methods <- methods[!(names(methods) %in% c("initialize","clone"))]
  for(i in 1:length(methods))
    assign(names(methods)[[i]],methods[[i]],envir=as.environment(private))

  invisible(self)
})

t = testMultiple$new(Continuous,Univariate)
t$printPublicForm()
t$printPublicSupport()
t$publicForm
t$publicSupport
t$.__enclos_env__$private$printPrivateForm()
t$.__enclos_env__$private$printPrivateSupport()
t$.__enclos_env__$private$privateForm
t$.__enclos_env__$private$privateSupport

