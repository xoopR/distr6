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
Convolution <- R6Class("Convolution",
  inherit = DistributionWrapper, lock_objects = FALSE,
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
      distlist <- list(dist1, dist2)
      names(distlist) <- makeUniqueNames(rsapply(distlist, "short_name", active = TRUE))

      if ((testDiscrete(dist1) & testContinuous(dist2)) |
          (testContinuous(dist1) & testDiscrete(dist2)) |
          testMixture(dist1) | testMixture(dist2)) {
        stop("Convolution not supported for mixed distributions.")
      }

      private$.outerParameters <- ParameterSet$new(
        id = "add", value = add, support = Logicals$new(),
        description = "Type of convolution."
      )

      super$initialize(
        distlist = distlist,
        name = paste0(dist1$name, "-", dist1$name, " Convolution"),
        short_name = paste0(dist1$short_name, dist2$short_name),
        description = paste("Convolution of", dist1$short_name, "and", dist2$short_name),
        support = dist2$traits$type,
        type = dist2$traits$type,
        valueSupport = dist2$traits$valueSupport,
        variateForm = dist2$traits$variateForm,
        outerID = "conv"
      )
    } # IN PROGRESS
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      d1 <- self$wrappedModels()[[1]]
      d2 <- self$wrappedModels()[[2]]
      add <- self$getParameterValue("conv_add")
      min <- max(d1$traits$type$lower, d2$traits$type$lower)
      max <- min(d1$traits$type$upper, d2$traits$type$upper)

      if (testContinuous(d1)) {
        if (add) {
          return(sapply(x, function(z) {
            integrate(
              f = function(y) d1$pdf(z - y) * d2$pdf(y),
              lower = min,
              upper = z - min
            )$value
          }))
        } else {
          return(sapply(x, function(z) {
            integrate(
              f = function(y) d1$pdf(y) * d2$pdf(y - z),
              lower = min + z,
              upper = max
            )$value
          }))
        }
      } else {
        if (add) {
          return(sapply(x, function(z) {
            ws <- d2$workingSupport()
            rng <- seq.int(ws$lower, ws$upper)
            rng <- rng[z - rng >= min & z - rng <= max]
            sum(d1$pdf(z - rng) * d2$pdf(rng))
          }))
        } else {
          stop("Substracting discrete random variables not currently supported.")
          # body(fnc) <- substitute({
          #   message(.distr6$message_numeric)
          #
          #   return(sapply(x1,function(z) {
          #     support <- try(self$wrappedModels(name1)$inf:self$wrappedModels(name1)$sup)
          #               if (inherits(support,"try-error")) {
          #                 self$wrappedModels(name2)$.__enclos_env__$private$.setWorkingSupport()
          #                 support <- self$wrappedModels(name2)$.__enclos_env__$
          #                 private$.getWorkingSupport()
          #                 support <- support$inf:support$sup
          #               }
          #     sum(self$wrappedModels(name1)$pdf(support-z) * self$wrappedModels(name2)$
          #     pdf(support))
          #   }))
          # },list(name1 = d1$short_name, name2 = d2$short_name))
        }
      }
    },
    .isCdf = 0L,
    .isQuantile = 0L,
    .isRand = 0L,
    .log = FALSE
  )
)
.distr6$wrappers <- append(.distr6$wrappers, list(Convolution = Convolution))

#' @rdname Convolution
#' @param x,y [Distribution]
#' @examples
#' binom <- Bernoulli$new() + Bernoulli$new()
#' binom$pdf(2)
#' Binomial$new(size = 2)$pdf(2)
#' @export
`+.Distribution` <- function(x, y) {
  Convolution$new(x, y, add = TRUE)
}
#' @rdname Convolution
#' @examples
#' norm <- Normal$new(mean = 3) - Normal$new(mean = 2)
#' norm$pdf(1)
#' Normal$new(mean = 1, var = 2)$pdf(1)
#' @export
`-.Distribution` <- function(x, y) {
  Convolution$new(x, y, add = FALSE)
}
