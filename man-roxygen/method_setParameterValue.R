#' @param ... \cr
#' Named arguments of parameters to set values for. See examples.
#' @param lst `(list(1))` \cr
#' Alternative argument for passing parameters. List names should be parameter names and list values
#' are the new values to set.
#' @param error `(character(1))` \cr
#' Passed to [stopwarn].
#' @examples
#' b = Binomial$new()
#' b$setParameterValue(size = 4, prob = 0.4)
#' b$setParameterValue(lst = list(size = 4, prob = 0.4))
