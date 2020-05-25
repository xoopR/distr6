isPdf <- function(dist) {
  assertDistribution(dist)
  dist$.__enclos_env__$private$.isPdf
}

isCdf <- function(dist) {
  assertDistribution(dist)
  dist$.__enclos_env__$private$.isCdf
}

isQuantile <- function(dist) {
  assertDistribution(dist)
  dist$.__enclos_env__$private$.isQuantile
}

isRand <- function(dist) {
  assertDistribution(dist)
  dist$.__enclos_env__$private$.isRand
}
