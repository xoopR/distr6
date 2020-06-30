#' @title Sample Empirical Distribution Without Replacement
#' @description Function to sample [Empirical] Distributions without replacement, as
#' opposed to the [rand] method which samples with replacement.
#' @param EmpiricalDist Empirical Distribution
#' @param n Number of samples to generate. See Details.
#' @param seed Numeric passed to \code{set.seed}. See Details.
#' @details This function can only be used to sample from the \code{Empirical} distribution without
#' replacement, and will return an error for other distributions.
#'
#' The `seed` param ensures that the same samples can be reproduced and is more convenient than
#' using the [set.seed()] function each time before use. If \code{set.seed} is \code{NULL} then the
#' seed is left unchanged (NULL is not passed to the \code{set.seed} function).
#'
#' If \code{n} is of length greater than one, then \code{n} is taken to be the length of \code{n}.
#' If \code{n} is greater than the number of observations in the Empirical distribution, then
#' \code{n} is taken to be the number of observations in the distribution.
#' @return A vector of length \code{n} with elements drawn without replacement from the given
#' Empirical distribution.
#' @export
simulateEmpiricalDistribution <- function(EmpiricalDist, n, seed = NULL) {

  if (!(getR6Class(EmpiricalDist) %in% c("Empirical", "EmpiricalMV"))) {
    stop("For Distributions that are not Empirical use $rand.")
  }

  if (!is.null(set.seed)) {
    set.seed(seed)
  }

  if (length(n) > 1) {
    n <- length(n)
  }

  if (getR6Class(EmpiricalDist) == "Empirical") {
    data <- EmpiricalDist$getParameterValue("data")
    if (n > nrow(data)) {
      n <- nrow(data)
    }
    return(sample(data$samples, n))
  } else {
    data <- EmpiricalDist$getParameterValue("data")
    if (n > nrow(data)) {
      n <- nrow(data)
    }
    dt <- data.table(apply(data, 2, function(x) sample(x, n)))
    colnames(dt) <- paste0("V", seq_along(dt))
    return(dt)
  }


}
