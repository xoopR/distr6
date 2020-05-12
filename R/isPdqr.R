isPdf = function(dist) {
  assertDistribution(dist)
  !is.null(dist$.__enclos_env__$private$.pdf)
}

isCdf = function(dist) {
  assertDistribution(dist)
  !is.null(dist$.__enclos_env__$private$.cdf)
}

isQuantile = function(dist) {
  assertDistribution(dist)
  !is.null(dist$.__enclos_env__$private$.quantile)
}

isRand = function(dist) {
  assertDistribution(dist)
  !is.null(dist$.__enclos_env__$private$.rand)
}
