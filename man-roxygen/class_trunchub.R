#' @param distribution `([Distribution])` \cr
#' [Distribution] to wrap.
#' @param lower `(numeric(1))` \cr
#' Lower limit to huberize the distribution at. If `NULL` then the lower bound of
#' the [Distribution] is used.
#' @param upper `(numeric(1))` \cr
#' Upper limit to huberize the distribution at. If `NULL` then the upper bound of
#' the [Distribution] is used.
#' @details
#' The pdf and cdf of the distribution are required for this wrapper, if unavailable decorate with
#' [FunctionImputation] first.
