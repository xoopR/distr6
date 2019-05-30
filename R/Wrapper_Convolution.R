Convolution <- R6::R6Class("Convolution", inherit = DistributionWrapper, lock_objects = FALSE)
Convolution$set("public","initialize",function(dist1, dist2, add = TRUE,
                                               type = NULL, ...){
  distlist = list(dist1$clone(), dist2$clone())
  distlist = makeUniqueDistributions(distlist)


  if(testContinuous(distlist$dist1) & testContinuous(distlist$dist2)){
    fnc <- function(x1) {}
    if(add){
      body(fnc) <- substitute({
        message("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x1,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(z - y)*
              self$getInternalModel(name2)$pdf(y)},
              lower = self$getInternalModel(name2)$inf(), upper = z)$value
        }))
      },list(name1 = distlist$dist1$short_name, name2 = distlist$dist2$short_name))
    } else {
      body(fnc) <- substitute({
        message("Results from numerical integration are approximate only, better results may be available.")
        return(sapply(x1,function(z){
          integrate(f = function(y){self$getInternalModel(name1)$pdf(y - z)*
              self$getInternalModel(name2)$pdf(y)},
              lower = self$getInternalModel(name2)$inf(),
              upper = z)$value
        }))
      },list(name1 = distlist$dist1$short_name, name2 = distlist$dist2$short_name))
    }
  } else if(testDiscrete(distlist$dist1) & testDiscrete(distlist$dist2)){
    fnc <- function(x1) {}
    if(add){
      body(fnc) <- substitute({
        return(sapply(x1,function(z){
          support <- try(self$getInternalModel(name2)$inf():self$getInternalModel(name2)$sup())
          if(inherits(support,"try-error"))
            support <- self$getInternalModel(name2)$.__enclos_env__$private$.getWorkingSupportRange()
          sum(self$getInternalModel(name1)$pdf(z - support) *
                self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = distlist$dist1$short_name, name2 = distlist$dist2$short_name))
    } else {
      body(fnc) <- substitute({
        return(sapply(x1,function(z){
          support <- try(self$getInternalModel(name2)$inf():self$getInternalModel(name2)$sup())
          if(inherits(support,"try-error"))
            support <- self$getInternalModel(name2)$.__enclos_env__$private$.getWorkingSupportRange()
          sum(self$getInternalModel(name1)$pdf(support - z) * self$getInternalModel(name2)$pdf(support))
        }))
      },list(name1 = distlist$dist1$short_name, name2 = distlist$dist2$short_name))
    }
  }

  name = paste("Convolution of",distlist$dist1$short_name,"and",distlist$dist2$short_name)
  short_name = paste0(distlist$dist1$short_name,distlist$dist2$short_name)

  if(testDiscrete(distlist$dist1) & testDiscrete(distlist$dist2))
    type = Naturals$new()
  else
    type = Reals$new()

  super$initialize(distlist = distlist, pdf = fnc, name = name,
                   short_name = short_name, type = type, ...)
}) # IN PROGRESS

`Distribution+Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = TRUE)
}
`Distribution-Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = FALSE)
}
