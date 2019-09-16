## Note the purpose of introducing Shift is to test how useful the wrapper is for shifting X to Y=aX+b
Shift <- R6::R6Class("Shift", inherit = DistributionWrapper, lock_objects = FALSE)
Shift$set("public","initialize",function(dist, a= 1,b=0,verbose=TRUE,...){
    assertDistribution(dist)
    dist = dist$clone(deep = TRUE)
    
    name = dist$name
    short_name = dist$short_name
    
    distlist = list(dist)
    names(distlist) = short_name
    
    if(is.null(a)){
        if(verbose == TRUE){
            a = 1 }else{
            message("a is set to be 1")
            a = 1 }
    }
    if(is.null(b)){
        if(verbose == TRUE){
            b = 0 }else{
            message("b is set to be 0")
            b = 0 }
    }
        
    private$.outerParameters <- ParameterSet$new(id = list("a","b"),
                                                 value = list(a, b),
                                                 support = list(Reals$new(),Reals$new()),
                                                 settable = list(FALSE, FALSE),
                                                 updateFunc = list(NA, NA),
                                                 description = list("a","b"))
    
    if(dist$.__enclos_env__$private$.isPdf){
        pdf <- function(x1){}
        body(pdf) <- substitute({
            self$wrappedModels()[[1]]$pdf(a*x1+b)
        }, list(name = short_name))
    } else
        pdf <- NULL
    
    if(dist$.__enclos_env__$private$.isCdf){
        cdf <- function(x1){}
        body(cdf) <- substitute({
            self$wrappedModels()[[1]]$cdf(a*x1+b)
        }, list(name = short_name))
    } else
        cdf <- NULL
    
    name = paste("Shifted",name)
    short_name = paste0("Shifted",short_name)
    
    super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                     short_name = short_name, type = Reals$new(),
                     support = Reals$new(),...)
}) # IN PROGRESS

Shift$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
    if(is.null(lst))
        lst <- list(...)
    super$setParameterValue(lst=lst,error=error)
})