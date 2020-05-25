#' @title Distribution Convolution Wrapper
#'
#' @description Calculates the convolution of two distribution via numerical calculations.
#' @template class_wrapper
#' @details The convolution of two probability distributions \eqn{X}, \eqn{Y} is the sum
#' \deqn{Z = X + Y}
#' which has a pmf,
#' \deqn{P(Z = z) = \sum_x P(X = x)P(Y = z - x)}
#' with an integration analogue for continuous distributions.
#'
#' Currently distr6 supports the addition of discrete and continuous probability distributions,
#' but only subtraction of continuous distributions.
#'
#' @return Returns an R6 object of class Convolution.
#'
#' @export
Convolution <- R6Class("Convolution", inherit = DistributionWrapper, lock_objects = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param dist1 `([Distribution])`\cr
    #' First [Distribution] in convolution, i.e. `dist1 ± dist2`.
    #' @param dist2 `([Distribution])`\cr
    #' Second [Distribution] in convolution, i.e. `dist1 ± dist2`.
    #' @param add `(logical(1))`\cr
    #' If `TRUE` (default) then adds the distributions together, otherwise substracts.
    initialize = function(dist1, dist2, add = TRUE) {
      distlist <- list(dist1$clone(), dist2$clone())
      distlist <- makeUniqueDistributions(distlist)

      if (testContinuous(distlist[[1]]) & testContinuous(distlist[[2]])) {
        fnc <- function(x1) {}
        if (add) {
          body(fnc) <- substitute(
            {
              message(.distr6$message_numeric)
              return(sapply(x1, function(z) {
                integrate(
                  f = function(y) {
                    self$wrappedModels(name1)$pdf(z - y) *
                      self$wrappedModels(name2)$pdf(y)
                  },
                  lower = -Inf,
                  upper = Inf
                )$value
              }))
            },
            list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name)
          )
        } else {
          body(fnc) <- substitute(
            {
              message(.distr6$message_numeric)
              return(sapply(x1, function(z) {
                integrate(
                  f = function(y) {
                    self$wrappedModels(name1)$pdf(y) *
                      self$wrappedModels(name2)$pdf(y - z)
                  },
                  lower = -Inf,
                  upper = Inf
                )$value
              }))
            },
            list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name)
          )
        }
      } else if (testDiscrete(distlist[[1]]) & testDiscrete(distlist[[2]])) {
        fnc <- function(x1) {}
        if (add) {
          body(fnc) <- substitute(
            {
              message(.distr6$message_numeric)

              return(sapply(x1, function(z) {
                support <- try(self$wrappedModels(name2)$inf:self$wrappedModels(name2)$sup, silent = T)
                if (inherits(support, "try-error")) {
                  self$wrappedModels(name2)$.__enclos_env__$private$.setWorkingSupport()
                  support <- self$wrappedModels(name2)$.__enclos_env__$private$.getWorkingSupport()
                  support <- support$inf:support$sup
                }
                sum(self$wrappedModels(name1)$pdf(z - support) *
                      self$wrappedModels(name2)$pdf(support))
              }))
            },
            list(name1 = distlist[[1]]$short_name, name2 = distlist[[2]]$short_name)
          )
        } else {
          stop("Substracting discrete random variables not currently supported.")
          # body(fnc) <- substitute({
          #   message(.distr6$message_numeric)
          #
          #   return(sapply(x1,function(z){
          #     support <- try(self$wrappedModels(name1)$inf:self$wrappedModels(name1)$sup)
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

      name <- paste("Convolution of", distlist[[1]]$short_name, "and", distlist[[2]]$short_name)
      short_name <- paste0(distlist[[1]]$short_name, distlist[[2]]$short_name)

      type <- distlist[[2]]$type
      valueSupport <- distlist[[2]]$valueSupport
      variateForm <- distlist[[2]]$variateForm

      super$initialize(
        distlist = distlist, pdf = fnc, name = name,
        short_name = short_name, type = type, valueSupport = valueSupport,
        variateForm = variateForm, support = type
      )
    } # IN PROGRESS
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(Convolution = Convolution))

`+.Distribution` <- function(dist1, dist2) {
  Convolution$new(dist1, dist2, add = TRUE)
}
`-.Distribution` <- function(dist1, dist2) {
  Convolution$new(dist1, dist2, add = FALSE)
}
