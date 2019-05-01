Convolution <- R6::R6Class("Convolution", inherit = DistributionWrapper)
Convolution$set("public","initialize",function(dist1, dist2, add = TRUE,...){
  distlist = list(dist1$clone(), dist2$clone())
  assertDistributionList(distlist)
  names(distlist) = c(dist1$short_name(), dist2$short_name())


  if(testContinuous(dist1) & testContinuous(dist2)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
        warning("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(z - y)*
              self$getInternalModel(name2)$pdf(y)},
              lower = self$getInternalModel(name2)$inf(), upper = z)$value
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    } else {
      body(fnc) <- substitute({
        warning("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(y - z)*
              self$getInternalModel(name2)$pdf(y)},
              lower = self$getInternalModel(name2)$inf(), upper = z)$value
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    }
  } else if(testDiscrete(dist1) & testDiscrete(dist2)){
    fnc <- function(x) {}
    if(add){
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(self$getInternalModel(name2)$inf(), z, by = 1)
          sum(self$getInternalModel(name1)$pdf(z - support) *
                self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    } else {
      body(fnc) <- substitute({
        return(sapply(x,function(z){
          support <- seq.int(self$getInternalModel(name2)$inf(), z, by = 1)
          sum(self$getInternalModel(name1)$pdf(support - z) * self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = dist1$short_name(), name2 = dist2$short_name()))
    }
  }

  name = paste("Convolution of",dist1$short_name(),"and",dist2$short_name())
  short_name = paste0(dist1$short_name(),dist2$short_name())

  super$initialize(distlist = distlist, pdf = fnc, name = name,
                   short_name = short_name, ...)
}) # IN PROGRESS

`+.Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = TRUE)
}
`-.Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = FALSE)
}