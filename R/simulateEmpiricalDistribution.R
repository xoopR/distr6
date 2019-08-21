simulateEmpiricalDistribution <- function(EmpiricalDist, n, seed = NULL){

  if(getR6Class(EmpiricalDist) != "Empirical")
    stop("For Distributions that are not Empirical use $rand.")

  if(is.null(seed))
    set.seed(stats::runif(1))
  else
    set.seed(seed)

  if(length(n) > 1)
    n <- length(n)

  return(sample(EmpiricalDist$support()$elements(), n))
}
