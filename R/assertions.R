#' @title assert/check/test/isDistribution
#' @description Validation checks to test if a given object is an R6 Distribution
#' @param x object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertDistribution(x)
#' checkDistribution(x)
#' testDistribution(x)
#' isDistribution(x)
#'
#' @examples
#' testDistribution(5) # FALSE
#' testDistribution(Binomial$new()) # TRUE
#'
#' @export
testDistribution <- function(){}
#' @rdname testDistribution
#' @export
checkDistribution <- function(){}
#' @rdname testDistribution
#' @export
assertDistribution <- function(){}
#' @rdname testDistribution
#' @export
isDistribution <- function(){}

makeChecks(assertionName = "Distribution",
           cond = inherits(x,"Distribution"),
           errormsg = paste(x,"is not an R6 Distribution object"),
           pos = environment())

#' @title assert/check/test/isDistributionList
#' @description Validation checks to test if a given object is a list of R6 Distributions
#' @param x object to test
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertDistributionList(x)
#' checkDistributionList(x)
#' testDistributionList(x)
#' isDistributionList(x)
#'
#' @examples
#' testDistributionList(list(Binomial$new(),5)) # FALSE
#' testDistributionList(list(Binomial$new(),Exponential$new())) # TRUE
#'
#' @export
testDistributionList <- function(){}
#' @rdname testDistributionList
#' @export
checkDistributionList <- function(){}
#' @rdname testDistributionList
#' @export
assertDistributionList <- function(){}
#' @rdname testDistributionList
#' @export
isDistributionList <- function(){}

makeChecks(assertionName =  "DistributionList",
           cond = all(unlist(lapply(x,inherits,"Distribution"))),
           errormsg = "One or more items in the list are not Distributions",
           pos = environment())

#' @title assert/check/test/isDistributionFeature
#' @description Validation checks to test for a given feature in a Distribution
#' @param x Distribution
#' @param accessor accessor of property/trait to test
#' @param feature feature to check if the property possesses
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertDistributionFeature(x, accessor, feature)
#' checkDistributionFeature(x, accessor, feature)
#' testDistributionFeature(x, accessor, feature)
#' isDistributionFeature(x, accessor, feature)
#'
#' @examples
#' testDistributionFeature(Binomial$new(), "variateForm", "multivariate") # TRUE
#' testDistributionFeature(Exponential$new(), "valueSupport", "discrete") # FALSE
#'
#' @export
testDistributionFeature <- function(){}
#' @rdname testDistributionFeature
#' @export
checkDistributionFeature <- function(){}
#' @rdname testDistributionFeature
#' @export
assertDistributionFeature <- function(){}
#' @rdname testDistributionFeature
#' @export
isDistributionFeature <- function(){}

makeChecks(assertionName =  "DistributionFeature",
           cond = x[[accessor]]() == feature,
           errormsg = paste(x$short_name,"is not",feature),
           args = alist(x=, accessor=, feature=),
           pos = environment())

#' @title assert/check/test/VariateForm
#' @name testVariateForm
#' @description Validation checks to test the variate form of a Distribution
#' @param x Distribution
#' @param type variate form type, univariate/multivariate/matrixvariate
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertVariateForm(x, type)
#' checkVariateForm(x, type)
#' testVariateForm(x, type)
#' isVariateForm(x, type)
#'
#' @examples
#' testVariateForm(Binomial$new(), "univariate") # silent
#' testVariateForm(Exponential$new(), "multivariate") # FALSE
#'
#' @export
testVariateForm <- function(){}
#' @rdname testVariateForm
#' @export
checkVariateForm <- function(){}
#' @rdname testVariateForm
#' @export
assertVariateForm <- function(){}
#' @rdname testVariateForm
#' @export
isVariateForm <- function(){}

makeChecks(assertionName =  "VariateForm",
           cond = x[["variateForm"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Univariate
#' @name testUnivariate
#' @description Validation checks to test if Distribution is univariate
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertUnivariate(x)
#' checkUnivariate(x)
#' testUnivariate(x)
#' isUnivariate(x)
#'
#' @examples
#' testUnivariate(Binomial$new()) # TRUE
#'
#' @export
testUnivariate <- function(){}
#' @rdname testUnivariate
#' @export
checkUnivariate <- function(){}
#' @rdname testUnivariate
#' @export
assertUnivariate <- function(){}
#' @rdname testUnivariate
#' @export
isUnivariate <- function(){}

makeChecks(assertionName =  "Univariate",
           cond = x[["variateForm"]]() == "univariate",
           errormsg = paste(x$short_name,"is not univariate"),
           pos = environment())

#' @title assert/check/test/Multivariate
#' @name testMultivariate
#' @description Validation checks to test if Distribution is multivariate.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertMultivariate(x)
#' checkMultivariate(x)
#' testMultivariate(x)
#' isMultivariate(x)
#'
#' @examples
#' testMultivariate(Binomial$new()) # FALSE
#'
#' @export
testMultivariate <- function(){}
#' @rdname testMultivariate
#' @export
checkMultivariate <- function(){}
#' @rdname testMultivariate
#' @export
assertMultivariate <- function(){}
#' @rdname testMultivariate
#' @export
isMultivariate <- function(){}

makeChecks(assertionName =  "Multivariate",
           cond = x[["variateForm"]]() == "multivariate",
           errormsg = paste(x$short_name,"is not multivariate"),
           pos = environment())

#' @title assert/check/test/Matrixvariate
#' @name testMatrixvariate
#' @description Validation checks to test if Distribution is matrixvariate.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertMatrixvariate(x)
#' checkMatrixvariate(x)
#' testMatrixvariate(x)
#' isMatrixvariate(x)
#'
#' @examples
#' testMatrixvariate(Binomial$new()) # FALSE
#'
#' @export
testMatrixvariate <- function(){}
#' @rdname testMatrixvariate
#' @export
checkMatrixvariate <- function(){}
#' @rdname testMatrixvariate
#' @export
assertMatrixvariate <- function(){}
#' @rdname testMatrixvariate
#' @export
isMatrixvariate <- function(){}

makeChecks(assertionName =  "Matrixvariate",
           cond = x[["variateForm"]]() == "matrixvariate",
           errormsg = paste(x$short_name,"is not matrixvariate"),
           pos = environment())

#' @title assert/check/test/ValueSupport
#' @name testValueSupport
#' @description Validation checks to test the value support of a Distribution
#' @param x Distribution
#' @param type value support type, continuous/discrete/mixture
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertValueSupport(x, type)
#' checkValueSupport(x, type)
#' testValueSupport(x, type)
#' isValueSupport(x, type)
#'
#' @examples
#' testValueSupport(Binomial$new(), "discrete") # silent
#' testValueSupport(Exponential$new(), "discrete") # FALSE
#'
#' @export
testValueSupport <- function(){}
#' @rdname testValueSupport
#' @export
checkValueSupport <- function(){}
#' @rdname testValueSupport
#' @export
assertValueSupport <- function(){}
#' @rdname testValueSupport
#' @export
isValueSupport <- function(){}

makeChecks(assertionName =  "ValueSupport",
           cond = x[["valueSupport"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Continuous
#' @name testContinuous
#' @description Validation checks to test if Distribution is continuous
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertContinuous(x)
#' checkContinuous(x)
#' testContinuous(x)
#' isContinuous(x)
#'
#' @examples
#' testContinuous(Binomial$new()) # FALSE
#'
#' @export
testContinuous <- function(){}
#' @rdname testContinuous
#' @export
checkContinuous <- function(){}
#' @rdname testContinuous
#' @export
assertContinuous <- function(){}
#' @rdname testContinuous
#' @export
isContinuous <- function(){}

makeChecks(assertionName =  "Continuous",
           cond = x[["valueSupport"]]() == "continuous",
           errormsg = paste(x$short_name,"is not continuous"),
           pos = environment())

#' @title assert/check/test/Discrete
#' @name testDiscrete
#' @description Validation checks to test if Distribution is discrete
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertDiscrete(x)
#' checkDiscrete(x)
#' testDiscrete(x)
#' isDiscrete(x)
#'
#' @examples
#' testDiscrete(Binomial$new()) # FALSE
#'
#' @export
testDiscrete <- function(){}
#' @rdname testDiscrete
#' @export
checkDiscrete <- function(){}
#' @rdname testDiscrete
#' @export
assertDiscrete <- function(){}
#' @rdname testDiscrete
#' @export
isDiscrete <- function(){}

makeChecks(assertionName =  "Discrete",
           cond = x[["valueSupport"]]() == "discrete",
           errormsg = paste(x$short_name,"is not discrete"),
           pos = environment())

#' @title assert/check/test/Mixture
#' @name testMixture
#' @description Validation checks to test if Distribution is mixture
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertMixture(x)
#' checkMixture(x)
#' testMixture(x)
#' isMixture(x)
#'
#' @examples
#' testMixture(Binomial$new()) # FALSE
#'
#' @export
testMixture <- function(){}
#' @rdname testMixture
#' @export
checkMixture <- function(){}
#' @rdname testMixture
#' @export
assertMixture <- function(){}
#' @rdname testMixture
#' @export
isMixture <- function(){}

makeChecks(assertionName =  "Mixture",
           cond = x[["valueSupport"]]() == "mixture",
           errormsg = paste(x$short_name,"is not mixture"),
           pos = environment())

#' @title assert/check/test/Symmetric
#' @name testSymmetric
#' @description Validation checks to test if Distribution is symmetric
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertSymmetric(x)
#' checkSymmetric(x)
#' testSymmetric(x)
#' isSymmetric(x)
#'
#' @examples
#' testSymmetric(Binomial$new()) # FALSE
#'
#' @export
testSymmetric <- function(){}
#' @rdname testSymmetric
#' @export
checkSymmetric <- function(){}
#' @rdname testSymmetric
#' @export
assertSymmetric <- function(){}
#' @rdname testSymmetric
#' @export
isSymmetric <- function(){}

makeChecks(assertionName =  "Symmetric",
           cond = x[["symmetry"]]()=="symmetric",
           errormsg = paste(x$short_name,"is not symmetric"),
           pos = environment())

#' @title assert/check/test/Skewness
#' @name testSkewness
#' @description Validation checks to test the skewness of a Distribution
#' @param x Distribution
#' @param type skewness type, Negative Skew/No Skew/Positive Skew
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertSkewness(x, type)
#' checkSkewness(x, type)
#' testSkewness(x, type)
#' isSkewness(x, type)
#'
#' @examples
#' testSkewness(Binomial$new(), "Negative Skew")
#' testSkewness(Binomial$new(), "Positive Skew")
#'
#' @export
testSkewness <- function(){}
#' @rdname testSkewness
#' @export
checkSkewness <- function(){}
#' @rdname testSkewness
#' @export
assertSkewness <- function(){}
#' @rdname testSkewness
#' @export
isSkewness <- function(){}

makeChecks(assertionName =  "Skewness",
           cond = x[["skewnessType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/NegativeSkew
#' @name testNegativeSkew
#' @description Validation checks to test if Distribution is negative skew.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertNegativeSkew(x)
#' checkNegativeSkew(x)
#' testNegativeSkew(x)
#' isNegativeSkew(x)
#'
#' @examples
#' testNegativeSkew(Binomial$new())
#'
#' @export
testNegativeSkew <- function(){}
#' @rdname testNegativeSkew
#' @export
checkNegativeSkew <- function(){}
#' @rdname testNegativeSkew
#' @export
assertNegativeSkew <- function(){}
#' @rdname testNegativeSkew
#' @export
isNegativeSkew <- function(){}

makeChecks(assertionName =  "NegativeSkew",
           cond = x[["skewnessType"]]() == "Negative Skew",
           errormsg = paste(x$short_name,"is not negative skew"),
           pos = environment())

#' @title assert/check/test/PositiveSkew
#' @name testPositiveSkew
#' @description Validation checks to test if Distribution is positive skew.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertPositiveSkew(x)
#' checkPositiveSkew(x)
#' testPositiveSkew(x)
#' isPositiveSkew(x)
#'
#' @examples
#' testPositiveSkew(Binomial$new())
#'
#' @export
testPositiveSkew <- function(){}
#' @rdname testPositiveSkew
#' @export
checkPositiveSkew <- function(){}
#' @rdname testPositiveSkew
#' @export
assertPositiveSkew <- function(){}
#' @rdname testPositiveSkew
#' @export
isPositiveSkew <- function(){}

makeChecks(assertionName =  "PositiveSkew",
           cond = x[["skewnessType"]]() == "Positive Skew",
           errormsg = paste(x$short_name,"is not positive skew"),
           pos = environment())

#' @title assert/check/test/NoSkew
#' @name testNoSkew
#' @description Validation checks to test if Distribution is no skew.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertNoSkew(x)
#' checkNoSkew(x)
#' testNoSkew(x)
#' isNoSkew(x)
#'
#' @examples
#' testNoSkew(Binomial$new())
#'
#' @export
testNoSkew <- function(){}
#' @rdname testNoSkew
#' @export
checkNoSkew <- function(){}
#' @rdname testNoSkew
#' @export
assertNoSkew <- function(){}
#' @rdname testNoSkew
#' @export
isNoSkew <- function(){}

makeChecks(assertionName =  "NoSkew",
           cond = x[["skewnessType"]]() == "No Skew",
           errormsg = paste(x$short_name,"is not no skew"),
           pos = environment())

#' @title assert/check/test/Kurtosis
#' @name testKurtosis
#' @description Validation checks to test the kurtosis of a Distribution
#' @param x Distribution
#' @param type kurtosis type, leptokurtic/mesokurtic/platykurtic
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertKurtosis(x, type)
#' checkKurtosis(x, type)
#' testKurtosis(x, type)
#' isKurtosis(x, type)
#'
#' @examples
#' testKurtosis(Binomial$new(), "leptokurtic")
#' testKurtosis(Binomial$new(), "platykurtic")
#'
#' @export
testKurtosis <- function(){}
#' @rdname testKurtosis
#' @export
checkKurtosis <- function(){}
#' @rdname testKurtosis
#' @export
assertKurtosis <- function(){}
#' @rdname testKurtosis
#' @export
isKurtosis <- function(){}

makeChecks(assertionName =  "Kurtosis",
           cond = x[["kurtosisType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Platykurtic
#' @name testPlatykurtic
#' @description Validation checks to test if Distribution is platykurtic.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertPlatykurtic(x)
#' checkPlatykurtic(x)
#' testPlatykurtic(x)
#' isPlatykurtic(x)
#'
#' @examples
#' testPlatykurtic(Binomial$new())
#'
#' @export
testPlatykurtic <- function(){}
#' @rdname testPlatykurtic
#' @export
checkPlatykurtic <- function(){}
#' @rdname testPlatykurtic
#' @export
assertPlatykurtic <- function(){}
#' @rdname testPlatykurtic
#' @export
isPlatykurtic <- function(){}

makeChecks(assertionName =  "Platykurtic",
           cond = x[["kurtosisType"]]() == "platykurtic",
           errormsg = paste(x$short_name,"is not platykurtic"),
           pos = environment())

#' @title assert/check/test/Mesokurtic
#' @name testMesokurtic
#' @description Validation checks to test if Distribution is mesokurtic.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertMesokurtic(x)
#' checkMesokurtic(x)
#' testMesokurtic(x)
#' isMesokurtic(x)
#'
#' @examples
#' testMesokurtic(Binomial$new())
#'
#' @export
testMesokurtic <- function(){}
#' @rdname testMesokurtic
#' @export
checkMesokurtic <- function(){}
#' @rdname testMesokurtic
#' @export
assertMesokurtic <- function(){}
#' @rdname testMesokurtic
#' @export
isMesokurtic <- function(){}

makeChecks(assertionName =  "Mesokurtic",
           cond = x[["kurtosisType"]]() == "mesokurtic",
           errormsg = paste(x$short_name,"is not mesokurtic"),
           pos = environment())

#' @title assert/check/test/Leptokurtic
#' @name testLeptokurtic
#' @description Validation checks to test if Distribution is leptokurtic.
#' @param x Distribution
#' @return If check passes then \code{assert} returns invisibly and \code{test}/\code{check}/\code{is}
#'   return \code{TRUE}. If check fails, \code{assert} stops code with error, \code{check} returns
#'   an error message as string, \code{test}/\code{is} return \code{FALSE}.
#'
#' @usage assertLeptokurtic(x)
#' checkLeptokurtic(x)
#' testLeptokurtic(x)
#' isLeptokurtic(x)
#'
#' @examples
#' testLeptokurtic(Binomial$new())
#'
#' @export
testLeptokurtic <- function(){}
#' @rdname testLeptokurtic
#' @export
checkLeptokurtic <- function(){}
#' @rdname testLeptokurtic
#' @export
assertLeptokurtic <- function(){}
#' @rdname testLeptokurtic
#' @export
isLeptokurtic <- function(){}

makeChecks(assertionName =  "Leptokurtic",
           cond = x[["kurtosisType"]]() == "leptokurtic",
           errormsg = paste(x$short_name,"is not leptokurtic"),
           pos = environment())
