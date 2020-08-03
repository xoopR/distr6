#' @title assert/check/test/Distribution
#' @description Validation checks to test if a given object is a [Distribution].
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testDistribution(5) # FALSE
#' testDistribution(Binomial$new()) # TRUE
#' @export
testDistribution <- function() {}
#' @rdname testDistribution
#' @export
checkDistribution <- function() {}
#' @rdname testDistribution
#' @export
assertDistribution <- function() {}

makeChecks(
  assertionName = "Distribution",
  cond = inherits(object, "Distribution"),
  defaulterrormsg = paste(object, "is not an R6 Distribution object"),
  pos = environment()
)

#' @title assert/check/test/DistributionList
#' @description Validation checks to test if a given object is a list of [Distribution]s.
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testDistributionList(list(Binomial$new(), 5)) # FALSE
#' testDistributionList(list(Binomial$new(), Exponential$new())) # TRUE
#' @export
testDistributionList <- function() {}
#' @rdname testDistributionList
#' @export
checkDistributionList <- function() {}
#' @rdname testDistributionList
#' @export
assertDistributionList <- function() {}

makeChecks(
  assertionName = "DistributionList",
  cond = checkmate::testList(object, "Distribution"),
  defaulterrormsg = "One or more items in the list are not Distributions",
  pos = environment()
)

#' @title assert/check/test/ParameterSet
#' @description Validation checks to test if a given object is a [ParameterSet].
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testParameterSet(5) # FALSE
#' testParameterSet(Binomial$new()$parameters()) # TRUE
#' @export
testParameterSet <- function() {}
#' @rdname testParameterSet
#' @export
checkParameterSet <- function() {}
#' @rdname testParameterSet
#' @export
assertParameterSet <- function() {}

makeChecks(
  assertionName = "ParameterSet",
  cond = inherits(object, "ParameterSet"),
  defaulterrormsg = paste(object, "is not an R6 ParameterSet object"),
  pos = environment()
)

#' @title assert/check/test/ParameterSetList
#' @description Validation checks to test if a given object is a list of [ParameterSet]s.
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testParameterSetList(list(Binomial$new(), 5)) # FALSE
#' testParameterSetList(list(Binomial$new(), Exponential$new())) # TRUE
#' @export
testParameterSetList <- function() {}
#' @rdname testParameterSetList
#' @export
checkParameterSetList <- function() {}
#' @rdname testParameterSetList
#' @export
assertParameterSetList <- function() {}

makeChecks(
  assertionName = "ParameterSetList",
  cond = checkmate::testList(object, "ParameterSet"),
  defaulterrormsg = "One or more items in the list are not ParameterSets",
  pos = environment()
)

#' @title assert/check/test/ParameterSetCollection
#' @description Validation checks to test if a given object is a [ParameterSetCollection].
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' # FALSE
#' testParameterSetCollection(5)
#' # TRUE
#' testParameterSetCollection(ParameterSetCollection$new(Binom = Binomial$new()$parameters()))
#' @export
testParameterSetCollection <- function() {}
#' @rdname testParameterSetCollection
#' @export
checkParameterSetCollection <- function() {}
#' @rdname testParameterSetCollection
#' @export
assertParameterSetCollection <- function() {}

makeChecks(
  assertionName = "ParameterSetCollection",
  cond = inherits(object, "ParameterSetCollection"),
  defaulterrormsg = paste(object, "is not an R6 ParameterSetCollection object"),
  pos = environment()
)

#' @title assert/check/test/ParameterSetCollectionList
#' @description Validation checks to test if a given object is a list of [ParameterSetCollection]s.
#' @param object object to test
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testParameterSetCollectionList(list(Binomial$new(), 5)) # FALSE
#' testParameterSetCollectionList(list(ParameterSetCollection$new(
#'   Binom = Binomial$new()$parameters()
#' ))) # TRUE
#' @export
testParameterSetCollectionList <- function() {} # nolint
#' @rdname testParameterSetCollectionList
#' @export
checkParameterSetCollectionList <- function() {} # nolint
#' @rdname testParameterSetCollectionList
#' @export
assertParameterSetCollectionList <- function() {} # nolint

makeChecks(
  assertionName = "ParameterSetCollectionList",
  cond = checkmate::testList(object, "ParameterSetCollection"),
  defaulterrormsg = "One or more items in the list are not ParameterSetCollections",
  pos = environment()
)

#' @title assert/check/test/Univariate
#' @name testUnivariate
#' @description Validation checks to test if Distribution is univariate.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testUnivariate(Binomial$new()) # TRUE
#' @export
testUnivariate <- function() {}
#' @rdname testUnivariate
#' @export
checkUnivariate <- function() {}
#' @rdname testUnivariate
#' @export
assertUnivariate <- function() {}

makeChecks(
  assertionName = "Univariate",
  cond = object$traits$variateForm == "univariate",
  defaulterrormsg = paste(object$short_name, "is not univariate"),
  pos = environment()
)

#' @title assert/check/test/Multivariate
#' @name testMultivariate
#' @description Validation checks to test if Distribution is multivariate.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testMultivariate(Binomial$new()) # FALSE
#' @export
testMultivariate <- function() {}
#' @rdname testMultivariate
#' @export
checkMultivariate <- function() {}
#' @rdname testMultivariate
#' @export
assertMultivariate <- function() {}

makeChecks(
  assertionName = "Multivariate",
  cond = object$traits$variateForm == "multivariate",
  defaulterrormsg = paste(object$short_name, "is not multivariate"),
  pos = environment()
)

#' @title assert/check/test/Matrixvariate
#' @name testMatrixvariate
#' @description Validation checks to test if Distribution is matrixvariate.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testMatrixvariate(Binomial$new()) # FALSE
#' @export
testMatrixvariate <- function() {}
#' @rdname testMatrixvariate
#' @export
checkMatrixvariate <- function() {}
#' @rdname testMatrixvariate
#' @export
assertMatrixvariate <- function() {}

makeChecks(
  assertionName = "Matrixvariate",
  cond = object$traits$variateForm == "matrixvariate",
  defaulterrormsg = paste(object$short_name, "is not matrixvariate"),
  pos = environment()
)

#' @title assert/check/test/Continuous
#' @name testContinuous
#' @description Validation checks to test if Distribution is continuous.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testContinuous(Binomial$new()) # FALSE
#' @export
testContinuous <- function() {}
#' @rdname testContinuous
#' @export
checkContinuous <- function() {}
#' @rdname testContinuous
#' @export
assertContinuous <- function() {}

makeChecks(
  assertionName = "Continuous",
  cond = object$traits$valueSupport == "continuous",
  defaulterrormsg = paste(object$short_name, "is not continuous"),
  pos = environment()
)

#' @title assert/check/test/Discrete
#' @name testDiscrete
#' @description Validation checks to test if Distribution is discrete.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testDiscrete(Binomial$new()) # FALSE
#' @export
testDiscrete <- function() {}
#' @rdname testDiscrete
#' @export
checkDiscrete <- function() {}
#' @rdname testDiscrete
#' @export
assertDiscrete <- function() {}

makeChecks(
  assertionName = "Discrete",
  cond = object$traits$valueSupport == "discrete",
  defaulterrormsg = paste(object$short_name, "is not discrete"),
  pos = environment()
)

#' @title assert/check/test/Mixture
#' @name testMixture
#' @description Validation checks to test if Distribution is mixture.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testMixture(Binomial$new()) # FALSE
#' @export
testMixture <- function() {}
#' @rdname testMixture
#' @export
checkMixture <- function() {}
#' @rdname testMixture
#' @export
assertMixture <- function() {}

makeChecks(
  assertionName = "Mixture",
  cond = object$traits$valueSupport == "mixture",
  defaulterrormsg = paste(object$short_name, "is not mixture"),
  pos = environment()
)

#' @title assert/check/test/Symmetric
#' @name testSymmetric
#' @description Validation checks to test if Distribution is symmetric.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testSymmetric(Binomial$new()) # FALSE
#' @export
testSymmetric <- function() {}
#' @rdname testSymmetric
#' @export
checkSymmetric <- function() {}
#' @rdname testSymmetric
#' @export
assertSymmetric <- function() {}

makeChecks(
  assertionName = "Symmetric",
  cond = object$properties$symmetry == "symmetric",
  defaulterrormsg = paste(object$short_name, "is not symmetric"),
  pos = environment()
)

#' @title assert/check/test/NegativeSkew
#' @name testNegativeSkew
#' @description Validation checks to test if Distribution is negative skew.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testNegativeSkew(Binomial$new())
#' @export
testNegativeSkew <- function() {}
#' @rdname testNegativeSkew
#' @export
checkNegativeSkew <- function() {}
#' @rdname testNegativeSkew
#' @export
assertNegativeSkew <- function() {}

makeChecks(
  assertionName = "NegativeSkew",
  cond = skewType(object$skewness()) == "Negative Skew",
  defaulterrormsg = paste(object$short_name, "is not negative skew"),
  pos = environment()
)

#' @title assert/check/test/PositiveSkew
#' @name testPositiveSkew
#' @description Validation checks to test if Distribution is positive skew.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testPositiveSkew(Binomial$new())
#' @export
testPositiveSkew <- function() {}
#' @rdname testPositiveSkew
#' @export
checkPositiveSkew <- function() {}
#' @rdname testPositiveSkew
#' @export
assertPositiveSkew <- function() {}

makeChecks(
  assertionName = "PositiveSkew",
  cond = skewType(object$skewness()) == "Positive Skew",
  defaulterrormsg = paste(object$short_name, "is not positive skew"),
  pos = environment()
)

#' @title assert/check/test/NoSkew
#' @name testNoSkew
#' @description Validation checks to test if Distribution is no skew.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#'
#' @examples
#' testNoSkew(Binomial$new())
#' @export
testNoSkew <- function() {}
#' @rdname testNoSkew
#' @export
checkNoSkew <- function() {}
#' @rdname testNoSkew
#' @export
assertNoSkew <- function() {}

makeChecks(
  assertionName = "NoSkew",
  cond = skewType(object$skewness()) == "No Skew",
  defaulterrormsg = paste(object$short_name, "is not no skew"),
  pos = environment()
)

#' @title assert/check/test/Platykurtic
#' @name testPlatykurtic
#' @description Validation checks to test if Distribution is platykurtic.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testPlatykurtic(Binomial$new())
#' @export
testPlatykurtic <- function() {}
#' @rdname testPlatykurtic
#' @export
checkPlatykurtic <- function() {}
#' @rdname testPlatykurtic
#' @export
assertPlatykurtic <- function() {}

makeChecks(
  assertionName = "Platykurtic",
  cond = exkurtosisType(object$kurtosis()) == "platykurtic",
  defaulterrormsg = paste(object$short_name, "is not platykurtic"),
  pos = environment()
)

#' @title assert/check/test/Mesokurtic
#' @name testMesokurtic
#' @description Validation checks to test if Distribution is mesokurtic.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testMesokurtic(Binomial$new())
#' @export
testMesokurtic <- function() {}
#' @rdname testMesokurtic
#' @export
checkMesokurtic <- function() {}
#' @rdname testMesokurtic
#' @export
assertMesokurtic <- function() {}

makeChecks(
  assertionName = "Mesokurtic",
  cond = exkurtosisType(object$kurtosis()) == "mesokurtic",
  defaulterrormsg = paste(object$short_name, "is not mesokurtic"),
  pos = environment()
)

#' @title assert/check/test/Leptokurtic
#' @name testLeptokurtic
#' @description Validation checks to test if Distribution is leptokurtic.
#' @param object Distribution
#' @param errormsg custom error message to return if assert/check fails
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test} returns \code{FALSE}.
#'
#' @examples
#' testLeptokurtic(Binomial$new())
#' @export
testLeptokurtic <- function() {}
#' @rdname testLeptokurtic
#' @export
checkLeptokurtic <- function() {}
#' @rdname testLeptokurtic
#' @export
assertLeptokurtic <- function() {}

makeChecks(
  assertionName = "Leptokurtic",
  cond = exkurtosisType(object$kurtosis()) == "leptokurtic",
  defaulterrormsg = paste(object$short_name, "is not leptokurtic"),
  pos = environment()
)
