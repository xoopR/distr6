#' @title assert/check/test/isDistribution
#' @name testDistribution
#' @aliases
#'   checkDistribution
#'   assertDistribution
#'   isDistribution
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
RSmisc::makeChecks(assertionName = "Distribution",
           cond = inherits(x,"Distribution"),
           errormsg = paste(x,"is not an R6 Distribution object"),
           pos = environment())

#' @title assert/check/test/isDistributionList
#' @name testDistributionList
#' @aliases
#'   checkDistributionList
#'   assertDistributionList
#'   isDistributionList
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
RSmisc::makeChecks(assertionName =  "DistributionList",
           cond = all(unlist(lapply(x,inherits,"Distribution"))),
           errormsg = "One or more items in the list are not Distributions",
           pos = environment())

#' @title assert/check/test/isDistributionFeature
#' @name testDistributionFeature
#' @aliases
#'   checkDistributionFeature
#'   assertDistributionFeature
#'   isDistributionFeature
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
RSmisc::makeChecks(assertionName =  "DistributionFeature",
           cond = x[[accessor]]() == feature,
           errormsg = paste(x$short_name,"is not",feature),
           args = alist(x=, accessor=, feature=),
           pos = environment())

#' @title assert/check/test/VariateForm
#' @name testVariateForm
#' @aliases
#'   checkVariateForm
#'   assertVariateForm
#'   isVariateForm
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
RSmisc::makeChecks(assertionName =  "VariateForm",
           cond = x[["variateForm"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Univariate
#' @name testUnivariate
#' @aliases
#'   checkUnivariate
#'   assertUnivariate
#'   isUnivariate
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
RSmisc::makeChecks(assertionName =  "Univariate",
           cond = x[["variateForm"]]() == "univariate",
           errormsg = paste(x$short_name,"is not univariate"),
           pos = environment())

#' @title assert/check/test/Multivariate
#' @name testMultivariate
#' @aliases
#'   checkMultivariate
#'   assertMultivariate
#'   isMultivariate
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
RSmisc::makeChecks(assertionName =  "Multivariate",
           cond = x[["variateForm"]]() == "multivariate",
           errormsg = paste(x$short_name,"is not multivariate"),
           pos = environment())

#' @title assert/check/test/Matrixvariate
#' @name testMatrixvariate
#' @aliases
#'   checkMatrixvariate
#'   assertMatrixvariate
#'   isMatrixvariate
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
RSmisc::makeChecks(assertionName =  "Matrixvariate",
           cond = x[["variateForm"]]() == "matrixvariate",
           errormsg = paste(x$short_name,"is not matrixvariate"),
           pos = environment())

#' @title assert/check/test/ValueSupport
#' @name testValueSupport
#' @aliases
#'   checkValueSupport
#'   assertValueSupport
#'   isValueSupport
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
RSmisc::makeChecks(assertionName =  "ValueSupport",
           cond = x[["valueSupport"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Continuous
#' @name testContinuous
#' @aliases
#'   checkContinuous
#'   assertContinuous
#'   isContinuous
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
RSmisc::makeChecks(assertionName =  "Continuous",
           cond = x[["valueSupport"]]() == "continuous",
           errormsg = paste(x$short_name,"is not continuous"),
           pos = environment())

#' @title assert/check/test/Discrete
#' @name testDiscrete
#' @aliases
#'   checkDiscrete
#'   assertDiscrete
#'   isDiscrete
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
RSmisc::makeChecks(assertionName =  "Discrete",
           cond = x[["valueSupport"]]() == "discrete",
           errormsg = paste(x$short_name,"is not discrete"),
           pos = environment())

#' @title assert/check/test/Mixture
#' @name testMixture
#' @aliases
#'   checkMixture
#'   assertMixture
#'   isMixture
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
RSmisc::makeChecks(assertionName =  "Mixture",
           cond = x[["valueSupport"]]() == "mixture",
           errormsg = paste(x$short_name,"is not mixture"),
           pos = environment())

#' @title assert/check/test/Symmetric
#' @name testSymmetric
#' @aliases
#'   checkSymmetric
#'   assertSymmetric
#'   isSymmetric
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
RSmisc::makeChecks(assertionName =  "Symmetric",
           cond = x[["symmetry"]]()=="symmetric",
           errormsg = paste(x$short_name,"is not symmetric"),
           pos = environment())

#' @title assert/check/test/Skewness
#' @name testSkewness
#' @aliases
#'   checkSkewness
#'   assertSkewness
#'   isSkewness
#' @description Validation checks to test the skewnewss of a Distribution
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
RSmisc::makeChecks(assertionName =  "Skewness",
           cond = x[["skewnessType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/NegativeSkew
#' @name testNegativeSkew
#' @aliases
#'   checkNegativeSkew
#'   assertNegativeSkew
#'   isNegativeSkew
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
RSmisc::makeChecks(assertionName =  "NegativeSkew",
           cond = x[["skewnessType"]]() == "Negative Skew",
           errormsg = paste(x$short_name,"is not negative skew"),
           pos = environment())

#' @title assert/check/test/PositiveSkew
#' @name testPositiveSkew
#' @aliases
#'   checkPositiveSkew
#'   assertPositiveSkew
#'   isPositiveSkew
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
RSmisc::makeChecks(assertionName =  "PositiveSkew",
           cond = x[["skewnessType"]]() == "Positive Skew",
           errormsg = paste(x$short_name,"is not positive skew"),
           pos = environment())

#' @title assert/check/test/NoSkew
#' @name testNoSkew
#' @aliases
#'   checkNoSkew
#'   assertNoSkew
#'   isNoSkew
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
RSmisc::makeChecks(assertionName =  "NoSkew",
           cond = x[["skewnessType"]]() == "No Skew",
           errormsg = paste(x$short_name,"is not no skew"),
           pos = environment())

#' @title assert/check/test/Kurtosis
#' @name testKurtosis
#' @aliases
#'   checkKurtosis
#'   assertKurtosis
#'   isKurtosis
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
RSmisc::makeChecks(assertionName =  "Kurtosis",
           cond = x[["kurtosisType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=),
           pos = environment())

#' @title assert/check/test/Platykurtic
#' @name testPlatykurtic
#' @aliases
#'   checkPlatykurtic
#'   assertPlatykurtic
#'   isPlatykurtic
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
RSmisc::makeChecks(assertionName =  "Platykurtic",
           cond = x[["kurtosisType"]]() == "platykurtic",
           errormsg = paste(x$short_name,"is not platykurtic"),
           pos = environment())

#' @title assert/check/test/Mesokurtic
#' @name testMesokurtic
#' @aliases
#'   checkMesokurtic
#'   assertMesokurtic
#'   isMesokurtic
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
RSmisc::makeChecks(assertionName =  "Mesokurtic",
           cond = x[["kurtosisType"]]() == "mesokurtic",
           errormsg = paste(x$short_name,"is not mesokurtic"),
           pos = environment())

#' @title assert/check/test/Leptokurtic
#' @name testLeptokurtic
#' @aliases
#'   checkLeptokurtic
#'   assertLeptokurtic
#'   isLeptokurtic
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
RSmisc::makeChecks(assertionName =  "Leptokurtic",
           cond = x[["kurtosisType"]]() == "leptokurtic",
           errormsg = paste(x$short_name,"is not leptokurtic"),
           pos = environment())
