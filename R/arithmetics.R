`+.Exponential` <- function(dist1, dist2){
  if(inherits(dist2,"Exponential")){
    checkmate::assert(dist1$getParameterValue("rate") == dist2$getParameterValue("rate"))
    Gamma$new(shape = 2, rate = dist1$getParameterValue("rate"))
  }
}

`+.Binomial` <- function(dist1, dist2,...){
  if(inherits(dist2, "Binomial") &  dist1$getParameterValue("prob") == dist2$getParameterValue("prob")){
    size = dist1$getParameterValue("size") + dist2$getParameterValue("size")
    prob = dist1$getParameterValue("prob")
    return(Binomial$new(size, prob))
  }
}
