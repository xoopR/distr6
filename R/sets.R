library(R6)

setSymbol <- function(set){
  if(!inherits(set,"character"))
    set = paste0(substitute(set))
  return(switch(set,
         naturals = "\u2115",
         posNaturals = "\u2115+",
         integers = "\u2124",
         posIntegers = "\u2124+",
         negIntegers = "\u2124-",
         rationals = "\u211A",
         posRationals = "\u211A+",
         negRationals = "\u211A-",
         reals = "\u211D",
         posReals = "\u211D+",
         negReals = "\u211D-",
         extendedReals = "\u211D \u222A {-\u221E, +\u221E}",
         complex = "\u2102"
  ))
}

set <- R6Class("set")
set$set("private",".setSymbol",NULL)
set$set("public","getSymbol",function() return(private$.setSymbol))
set$set("public","initialize",function(...){
  if(missing(...))
    invisible(self)
  else{
    private$.setSymbol <- paste0("{",paste(...,sep = ", "),"}")
  }
})
set$set("public","print",function(){
  print(self$getSymbol())
})

specialSet <- R6Class("specialSet", inherit = set)
specialSet$set("public","initialize",function(...){
    private$.setSymbol <- setSymbol(paste0(getR6Class(self)))
    invisible(self)
})

naturals <- R6Class("naturals",inherit = specialSet)
posNaturals <- R6Class("posNaturals",inherit = naturals)

integers <- R6Class("integers",inherit = specialSet)
posIntegers <- R6Class("posIntegers",inherit = integers)
negIntegers <- R6Class("negIntegers",inherit = integers)

rationals <- R6Class("rationals",inherit = specialSet)
posRationals <- R6Class("posRationals",inherit = rationals)
negRationals <- R6Class("negRationals",inherit = rationals)

reals <- R6Class("reals",inherit = specialSet)
posReals <- R6Class("posReals",inherit = reals)
negReals <- R6Class("negReals",inherit = reals)
extendedReals <- R6Class("extendedReals",inherit = reals)

complex <- R6Class("complex",inherit = specialSet)

interval <- R6Class("interval")
interval$set("private",".setSymbol",NULL)
interval$set("public","getSymbol",function() return(private$.setSymbol))
interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]"){
  types = c("()","(]","[]","[)")
  stopifnot(type %in% types)
  stopifnot(lower<=upper)
  if(lower == -Inf) lower = "-\u221E"
  if(upper == Inf) upper = "+\u221E"
  private$.setSymbol <- paste0(substr(type,1,1),lower,":", upper,substr(type,2,2))
  invisible(self)
})
interval$set("public","print",function(){
  print(self$getSymbol())
})

operation <- function(unicode,...){
  dots = list(...)
  symbols = lapply(dots,function(x){
    if(inherits(x,"R6ClassGenerator"))
      x <- x[["new"]]()
    x <- x[["getSymbol"]]()
    if(!grepl("\\{.",x))
      x <- paste0("{", x)
    if(!grepl(".\\}",x))
      x <- paste0(x,"}")
    return(x)
  })

  setSymbol <- paste(unlist(symbols), collapse = paste0(" ",unicode," "))
  return(set$new(setSymbol))
}
product <- function(...){
  operation("\u00D7",...)
}
union <- function(...){
  operation("\u222A",...)
}