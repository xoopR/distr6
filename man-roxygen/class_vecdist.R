#' @param distlist `(list())`\cr
#' List of [Distribution]s.
#' @param distribution `(character(1))` \cr
#' Should be supplied with `params` and optionally `shared_params` as an alternative to `distlist`.
#' Much faster implementation when only one class of distribution is being wrapped. `distribution`
#' is the full name of one of the distributions in [listDistributions()], or `"Distribution"` if
#' constructing custom distributions. See examples in [VectorDistribution].
#' @param params `(list()|data.frame())` \cr
#' Parameters in the individual distributions for use with `distribution`. Can be supplied as a list,
#' where each element is the list of parameters to set in the distribution, or as an object
#' coercable to `data.frame`, where each column is a parameter and each row is a distribution.
#' See examples in [VectorDistribution].
#' @param shared_params `(list())`\cr
#' If any parameters are shared when using the `distribution` constructor, this provides a much faster
#' implementation to list and query them together. See examples in [VectorDistribution].
#' @param name `(character(1))`\cr
#' Optional name of wrapped distribution.
#' @param short_name `(character(1))`\cr
#' Optional short name/ID of wrapped distribution.
#' @param vecdist [VectorDistribution] \cr
#' Alternative constructor to directly create this object from an object inheriting from
#' [VectorDistribution].
#'
#' @family wrappers
