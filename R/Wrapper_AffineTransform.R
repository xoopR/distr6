#' @title Distribution Affine Transformer Wrapper
#' @name AffineTransform
#' @export
NULL
AffineTransform <- R6::R6Class("AffineTransform", inherit = DistributionWrapper, lock_objects = FALSE)
AffineTransform$set("public","initialize",function(dist, a= 1,b=0,verbose=TRUE,...){
    assertDistribution(dist)
    dist = dist$clone(deep = TRUE)

    name = dist$name
    short_name = dist$short_name

    distlist = list(dist)
    names(distlist) = short_name
    
    # type setting
    if(class(dist$support())==class(Binomial$new(prob = 0.1,size = 10)$support()) ){
        
        min1 = dist$support()$min()
        max1 = dist$support()$max()
        support1 = Set$new(min1:max1)
        
    }else if(class(dist$support())==class(Normal$new(mean = 2,sd=2)$support()) ){
        
        support1 = Reals$new()
        
    }else if(class(dist$support())==class(Exponential$new(0.3)$support()){
        
        if(b<0){
            support1 = Reals$new()
        }else if(b>=0){
            support1 = PosReals$new()
        }
        
    }
    
    if(a==0){stop("a cannot be zero",call. = TRUE)}

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

    if(dist$isPdf){
        pdf <- function(x1){}
        body(pdf) <- substitute({
            self$wrappedModels()[[1]]$pdf((x1 - b)/a)/abs(a)
        }, list(name = short_name))
    } else
        pdf <- NULL

    if(dist$isCdf){
        cdf <- function(x1){}
        body(cdf) <- substitute({
            if(a>0){
                self$wrappedModels()[[1]]$cdf((x1 - b)/a)
            }else if(a<0){
                1-self$wrappedModels()[[1]]$cdf((x1 - b)/a)
            }
        }, list(name = short_name))
    } else
        cdf <- NULL

    name = paste("AffineTransformed",name)
    short_name = paste0("AffineTransformed",short_name)

    super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, name = name,
                     short_name = short_name, type = Reals$new(),
                     support = support1,...)
}) # IN PROGRESS

AffineTransform$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
    if(is.null(lst))
        lst <- list(...)
    super$setParameterValue(lst=lst,error=error)

})




