
SDistribution <- R6::R6Class("SDistribution", inherit = Distribution)

SDistribution$set("public","setParameterValue",function(lst, error = "warn"){
  lst <- private$.getRefParams(lst)
  super$setParameterValue(lst, error)
})
