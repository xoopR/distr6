#' @title Helper Functionality for Constructing Distributions
#' @description Helper functions for constructing an [SDistribution] (with `dstr`)
#'  or [VectorDistribution] (with `dstrs`).
#' @aliases dstrs
#' @param d (`character(1)`) \cr
#' Distribution. Can be the `ShortName` or `ClassName` from [listDistributions()].
#' @param ... (`ANY`) \cr
#' Passed to the distribution constructor, should be parameters or `decorators`.
#' @param pars (`list()`) \cr
#' List of parameters of same length as `d` corresponding to distribution parameters.
#' @param decorators (`character()`) \cr
#' Passed to distribution constructor.
#' @examples
#' # Construct standard Normal and  distribution
#' dstr("Norm") # ShortName
#' dstr("Normal") # ClassName
#'
#' # Construct Binomial(5, 0.1)
#' dstr("Binomial", size = 5, prob = 0.1)
#'
#' # Construct decorated Gamma(2, 1)
#' dstr("Gamma", shape = 2, rate = 1,
#'      decorators = "ExoticStatistics")
#'
#' # Or with a list
#' dstr("Gamma", pars = list(shape = 2, rate = 4))
#'
#' # Construct vector with dstrs
#'
#' # Binomial and Gamma with default parameters
#' dstrs(c("Binom", "Gamma"))
#'
#' # Binomial with set parameters and Gamma with
#' #  default parameters
#' dstrs(c("Binom", "Gamma"), list(list(size = 4), NULL))
#'
#' # Binomial and Gamma with set parameters
#' dstrs(c("Binom", "Gamma"),
#'      list(list(size = 4), list(rate = 2, shape = 3)))
#'
#' # Multiple Binomials
#' dstrs("Binom", data.frame(size = 1:5, prob = 0.5))
#'
#' @export
dstr <- function(d, ..., pars = list(...), decorators = NULL) {
  choices <- listDistributions()
  short <- as.character(unlist(choices$ShortName))
  class <- as.character(unlist(choices$ClassName))
  checkmate::assert_choice(d, c(short, class))

  ShortName <- ClassName <- NULL # global binding fix
  if (d %in% short) {
    do.call(get(as.character(unlist(subset(choices, ShortName == d, select = ClassName))))$new,
            c(pars, list(decorators = decorators)))
  } else if (d %in% class) {
    do.call(get(as.character(unlist(subset(choices, ClassName == d, select = ClassName))))$new,
            c(pars, list(decorators = decorators)))
  }
}

#' @rdname dstr
#' @export
dstrs <- function(d, pars = NULL, ...) {
  if (length(d) == 1) {
    if (is.null(pars)) {
      stop("pars' cannot be NULL if 'd' is length 1.")
    } else {
      VectorDistribution$new(distribution = d, params = pars, ...)
    }
  } else {
    if (is.null(pars)) {
      pars <- vector("list", length(d))
    }
    VectorDistribution$new(mapply(dstr, d, pars = pars, ...))
  }
}


#' @title Helper Functionality for Getting and Setting Distribution Parameters
#' @description Simple wrapper around d$getParameterValue(p) and
#'  d$setParameterValue(lst).
#' @param d (`Distribution(1)`) \cr
#' Distribution object.
#' @param p (`character()`) \cr
#' Name(s) of parameters to written.
#' @param lst (`list(1)`) \cr
#' Parameters to update.
#' @examples
#' d <- dstr("Norm")
#' gprm(d, "mean")
#' gprm(d, c("mean", "var"))
#' sprm(d, list(mean = 1, var = 3))
#' gprm(d, c("mean", "sd"))
#' @export
gprm <- function(d, p) {
  d$getParameterValue(p)
}

#' @rdname gprm
#' @export
sprm <- function(d, lst) {
  d$setParameterValue(lst = lst)
}