#' @title Distribution Convolution Wrapper
#'
#' @description Calculates the convolution of two distribution via numerical calculations.
#'
#' @details The convolution of two probability distributions \eqn{X}, \eqn{Y} is the sum
#' \deqn{Z = X + Y}
#' which has a pmf,
#' \deqn{P(Z = z) = \sum_x P(X = x)P(Y = z - x)}
#' with an integration analogue for continuous distributions.
#'
#' Currently distr6 supports the addition of discrete and continuous probability distributions, but only
#' subtraction of continuous distributions.
#'
#' @name Convolution
#'
#' @section Constructor: Convolution$new(dist1, dist2, add = TRUE, type = NULL)
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{dist1} \tab distribution \tab First distribution in convolution. \cr
#' \code{dist2} \tab distribution \tab Second distribution in convolution. \cr
#' \code{add} \tab logical \tab Add or subtract distributions. \cr
#' \code{type} \tab logical \tab Type of new distribution, automated if NULL. \cr
#' }
#'
#' @return Returns an R6 object of class Convolution.
#'
#' @seealso \code{\link{listWrappers}}
#'
#' @export
NULL
Convolution <- R6::R6Class("Convolution", inherit = DistributionWrapper, lock_objects = FALSE)
.distr6$wrappers <- append(.distr6$wrappers, list(Convolution = Convolution))

Convolution$set("public","initialize",function(dist1, dist2, add = TRUE,
                                               type = NULL){
  distlist = list(dist1$clone(), dist2$clone())
  distlist = makeUniqueDistributions(distlist)

  if(testContinuous(distlist[[1]]) & testContinuous(distlist[[2]])){
    fnc <- function(x1) {}
    if(add){
      body(fnc) <- substitute({
        message(.distr6$message_numeric)
        return(sapply(x1,function(z){
          integrate(f = function(y){self$wrappedModels(name1)$pdf(z - y)*
              self$wrappedModels(name2)$pdf(y)},
              lower = -Inf,
              upper = Inf)$value
        }))
      },list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name))
    } else {
      body(fnc) <- substitute({
        message(.distr6$message_numeric)
        return(sapply(x1,function(z){
          integrate(f = function(y){self$wrappedModels(name1)$pdf(y)*
              self$wrappedModels(name2)$pdf(y - z)},
              lower = -Inf,
              upper = Inf)$value
        }))
      },list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name))
    }
  } else if(testDiscrete(distlist[[1]]) & testDiscrete(distlist[[2]])){
    fnc <- function(x1) {}
    if(add){
      body(fnc) <- substitute({
        message(.distr6$message_numeric)

        return(sapply(x1,function(z){
          support <- try(self$wrappedModels(name2)$inf():self$wrappedModels(name2)$sup(), silent = T)
          if(inherits(support,"try-error")){
            self$wrappedModels(name2)$.__enclos_env__$private$.setWorkingSupport()
            support <- self$wrappedModels(name2)$.__enclos_env__$private$.getWorkingSupport()
            support <- support$inf:support$sup
          }
          sum(self$wrappedModels(name1)$pdf(z - support) *
                self$wrappedModels(name2)$pdf(support))
        }))
      },list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name))
    } else {
      stop("Substracting discrete random variables not currently supported.")
      # body(fnc) <- substitute({
      #   message(.distr6$message_numeric)
      #
      #   return(sapply(x1,function(z){
      #     support <- try(self$wrappedModels(name1)$inf():self$wrappedModels(name1)$sup())
      #               if(inherits(support,"try-error")){
      #                 self$wrappedModels(name2)$.__enclos_env__$private$.setWorkingSupport()
      #                 support <- self$wrappedModels(name2)$.__enclos_env__$private$.getWorkingSupport()
      #                 support <- support$inf:support$sup
      #               }
      #     sum(self$wrappedModels(name1)$pdf(support-z) * self$wrappedModels(name2)$pdf(support))
      #   }))
      # },list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name))
    }
  }

  name = paste("Convolution of",distlist[[1]]$short_name,"and",distlist[[2]]$short_name)
  short_name = paste0(distlist[[1]]$short_name,distlist[[2]]$short_name)

  type = distlist[[2]]$type()

  super$initialize(distlist = distlist, pdf = fnc, name = name,
                   short_name = short_name, type = type)
}) # IN PROGRESS

`+.Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = TRUE)
}
`-.Distribution` <- function(dist1, dist2){
  Convolution$new(dist1, dist2, add = FALSE)
}
