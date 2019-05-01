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

sets <- R6::R6Class("sets")
sets$set("private",".lower",NULL)
sets$set("private",".upper",NULL)
sets$set("private",".type",NULL)
sets$set("private",".dimension",NULL)
sets$set("public","lower",function(){
  return(private$.lower)
})
sets$set("public","upper",function(){
  return(private$.upper)
})
sets$set("public","type",function(){
  return(private$.type)
})
sets$set("public","dimension",function(){
  return(private$.dimension)
})
sets$set("public","max",function(){
  if(private$.type %in% c("()","[)"))
    return("Interval is not bounded above.")
  else
    return(self$upper())
})
sets$set("public","min",function(){
  if(private$.type %in% c("()","(]"))
    return("Interval is not bounded below.")
  else
    return(self$lower())
})
sets$set("public","sup",function(){
  return(self$upper())
})
sets$set("public","inf",function(){
  return(self$lower())
})
sets$set("public","strprint",function(){
  return(private$.setSymbol)
})
sets$set("public","power",function(x){
  private$.dimension <- private$.dimension*x
  private$.setSymbol <- paste0(setSymbol(paste0(getR6Class(self))),"^",self$dimension())
  invisible(self)
})

SymbolicSet <- R6::R6Class("SymbolicSet", inherit = sets)
SymbolicSet$set("private",".setSymbol",NULL)
SymbolicSet$set("public","getSymbol",function() return(private$.setSymbol))
SymbolicSet$set("public","initialize",function(...){
  if(missing(...))
    invisible(self)
  else{
    dots <- list(...)
    private$.setSymbol <- paste0("{",paste(dots,collapse = ", "),"}")
    private$.type <- "{}"
    private$.lower <- dots[[1]]
    private$.upper <- dots[[length(dots)]]
  }
})
SymbolicSet$set("public","print",function(){
  print(self$getSymbol())
})

specialSet <- R6::R6Class("specialSet", inherit = SymbolicSet)
specialSet$set("public","initialize",function(dim = 1, lower = -Inf,
                                              upper = Inf, type = "()", ...){
  if(dim!=1)
    private$.setSymbol <- paste0(setSymbol(paste0(getR6Class(self))),"^",dim)
  else
    private$.setSymbol <- setSymbol(paste0(getR6Class(self)))
  private$.lower <- lower
  private$.upper <- upper
  private$.type <- type
  private$.dimension <- dim
  invisible(self)
})

naturals <- R6::R6Class("naturals",inherit = specialSet)
naturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 0, type = "[)")
})
posNaturals <- R6::R6Class("posNaturals",inherit = specialSet)
posNaturals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})

integers <- R6::R6Class("integers",inherit = specialSet)
posIntegers <- R6::R6Class("posIntegers",inherit = specialSet)
posIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})
negIntegers <- R6::R6Class("negIntegers",inherit = specialSet)
negIntegers$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

rationals <- R6::R6Class("rationals",inherit = specialSet)
posRationals <- R6::R6Class("posRationals",inherit = rationals)
posRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})
negRationals <- R6::R6Class("negRationals",inherit = rationals)
negRationals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})

reals <- R6::R6Class("reals",inherit = specialSet)
posReals <- R6::R6Class("posReals",inherit = reals)
posReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, lower = 1e-09, type = "[)")
})
negReals <- R6::R6Class("negReals",inherit = reals)
negReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, upper = -1e-09, type = "(]")
})
extendedReals <- R6::R6Class("extendedReals",inherit = reals)
extendedReals$set("public", "initialize", function(dim = 1){
  super$initialize(dim, type = "[]")
})

complex <- R6::R6Class("complex",inherit = specialSet)

interval <- R6::R6Class("interval", inherit = sets)
interval$set("private",".setSymbol",NULL)
interval$set("public","getSymbol",function() return(private$.setSymbol))
interval$set("public","initialize",function(lower = -Inf, upper = Inf, type = "[]"){
  types = c("()","(]","[]","[)")
  stopifnot(type %in% types)
  stopifnot(lower<=upper)
  private$.lower = lower
  private$.upper = upper
  private$.type = type
  if(lower == -Inf) lower = "-\u221E"
  if(upper == Inf) upper = "+\u221E"
  private$.setSymbol <- paste0(substr(type,1,1),lower,":", upper,substr(type,2,2))
  invisible(self)
})
interval$set("public","print",function(){
  print(self$getSymbol())
})
interval$set("public","lower",function(){
  return(private$.lower)
})
interval$set("public","upper",function(){
  return(private$.upper)
})
interval$set("public","type",function(){
  return(private$.type)
})
interval$set("public","max",function(){
  if(type %in% c("()","[)"))
    return("Interval is not bounded above.")
  else
    return(self$upper())
})
interval$set("public","min",function(){
  if(type %in% c("()","(]"))
    return("Interval is not bounded below.")
  else
    return(self$lower())
})
interval$set("public","sup",function(){
  return(self$upper())
})
interval$set("public","inf",function(){
  return(self$lower())
})
interval$set("public","numeric",function(){
  if(self$type() == "[]")
    return(seq.int(self$lower(),self$upper(),1))
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
  return(SymbolicSet$new(setSymbol))
}
product <- function(...){
  operation("\u00D7",...)
}
union <- function(...){
  operation("\u222A",...)
}

`^.sets` <- function(x, power){
  x$power(power)
}
`+.sets` <- function(x, y){
  union(x, y)
}
`*.sets` <- function(x, y){
  product(x, y)
}
