#' @title Generalised Validaton Functions
#'
#' @description These functions are generalised and simplified based on the package \code{checkmate}.
#'   They are used primarily for the function \code{\link{makeChecks}} that automatically generates
#'   checkmate-style custom validation functions.
#'
#' @return \code{assertThat} returns self invisibly if checks pass, otherwise error
#' @usage assertThat(x, cond, errormsg)
#'
#' @param x Object to perform checks on
#' @param cond Boolean condition to check
#' @param errormsg Error message to produce
#'
#' @seealso \code{\link[checkmate]{assert}}
#'
#' @examples
#' x = 1:10
#' cond = inherits(x, "integer")
#' errormsg = "Not integer"
#' assertThat(x, cond, errormsg)
#' checkThat(cond, errormsg)
#' testThat(cond)
#' isThat(cond)
#'
#' @export
assertThat <- function(x, cond, errormsg){
  if(cond)
    invisible(x)
  else
    stop(errormsg)
}

#' @name checkThat
#' @rdname assertThat
#' @return \code{checkThat} returns TRUE if checks pass, otherwise error message as string
#' @usage checkThat(cond, errormsg)
#' @export
checkThat <- function(cond, errormsg){
  if(cond)
    return(TRUE)
  else
    return(errormsg)
}

#' @name testThat
#' @rdname assertThat
#' @return \code{testThat} returns TRUE if checks pass, otherwise FALSE
#' @usage testThat(cond)
#' @export
testThat <- function(cond){
  if(cond)
    return(TRUE)
  else
    return(FALSE)
}

#' @name isThat
#' @rdname assertThat
#' @return \code{isThat} returns TRUE if checks pass, otherwise FALSE
#' @usage isThat(cond)
#' @export
isThat <- function(cond){
  return(testThat(cond))
}

#' @title Automatic Generation of Validaton Functions
#'
#' @description This function uses the generalised assertion functions to assign assert/check/test/is
#' functions to the given environment for quick deployment of validation checks. See examples
#' for how it is used in distr6.
#'
#' @usage makeChecks(assertionName, cond, errormsg, args = alist(x=),
#'   pos = parent.env(environment()))
#'
#' @param assertionName name that follows assert/check/test/is
#' @param cond Boolean condition to check
#' @param errormsg Error message to produce
#' @param args Generic argument names for object to validate. See Details.
#' @param pos Environment position to assign functions to. See Details.
#'
#' @examples
#'  makeChecks(assertionName = "Numeric",
#'      cond = inherits(x,"numeric"),
#'      errormsg = paste(x,"is not numeric"), pos = 1)
#'  assertNumeric(as.numeric(0)) # silent
#'  checkNumeric(as.numeric(1)) # TRUE
#'  testNumeric("a") # FALSE
#'  isNumeric("2") # FALSE
#'
#' @details This generates simplified version of more complex assertion and validation
#'   functions that can be found in libraries such as \code{\link{checkmate}}. The purpose
#'   of which is to easily define validation checks that are used throughout distr6.
#'
#'   By default, validations are only made in relation to one argument however this can be
#'    extended by adding arguments to the \code{args} parameter. For example, \code{args = alist(x=, y=,...)}.
#'    Validation functions are assigned to the current parent environment, which is often .GlobalEnv
#'    however when loading and attaching libraries, it is the current library being loaded.
#'
#' @export
makeChecks <- function(assertionName, cond, errormsg, args = alist(x=),
                       pos = parent.env(environment())){
  cond = substitute(cond)
  errormsg = substitute(errormsg)
  value = function(x){}
  formals(value) = args
  body(value) = substitute(assertThat(x,arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("assert",assertionName), value = value,
         pos = pos)

  body(value) = substitute(checkThat(arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("check",assertionName), value = value,
         pos = pos)

  body(value) = substitute(testThat(arg1),list(arg1=cond))
  assign(paste0("test",assertionName), value = value,
         pos = pos)

  body(value) = substitute(isThat(arg1),list(arg1=cond))
  assign(paste0("is",assertionName), value = value,
         pos = pos)
}

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
makeChecks(assertionName = "Distribution",
           cond = inherits(x,"Distribution"),
           errormsg = paste(x,"is not an R6 Distribution object"))

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
makeChecks(assertionName =  "DistributionList",
           cond = all(unlist(lapply(x,inherits,"Distribution"))),
           errormsg = "One or more items in the list are not Distributions")

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
makeChecks(assertionName =  "DistributionFeature",
           cond = x[[accessor]]() == feature,
           errormsg = paste(x$short_name,"is not",feature),
           args = alist(x=, accessor=, feature=))

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
makeChecks(assertionName =  "VariateForm",
           cond = x[["variateForm"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=))

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
makeChecks(assertionName =  "Univariate",
           cond = x[["variateForm"]]() == "univariate",
           errormsg = paste(x$short_name,"is not univariate"))

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
makeChecks(assertionName =  "Multivariate",
           cond = x[["variateForm"]]() == "multivariate",
           errormsg = paste(x$short_name,"is not multivariate"))

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
makeChecks(assertionName =  "Matrixvariate",
           cond = x[["variateForm"]]() == "matrixvariate",
           errormsg = paste(x$short_name,"is not matrixvariate"))

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
makeChecks(assertionName =  "ValueSupport",
           cond = x[["valueSupport"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=))

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
makeChecks(assertionName =  "Continuous",
           cond = x[["valueSupport"]]() == "continuous",
           errormsg = paste(x$short_name,"is not continuous"))

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
makeChecks(assertionName =  "Discrete",
           cond = x[["valueSupport"]]() == "discrete",
           errormsg = paste(x$short_name,"is not discrete"))

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
makeChecks(assertionName =  "Mixture",
           cond = x[["valueSupport"]]() == "mixture",
           errormsg = paste(x$short_name,"is not mixture"))

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
makeChecks(assertionName =  "Symmetric",
           cond = x[["symmetry"]]()=="symmetric",
           errormsg = paste(x$short_name,"is not symmetric"))

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
makeChecks(assertionName =  "Skewness",
           cond = x[["skewnessType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=))

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
makeChecks(assertionName =  "NegativeSkew",
           cond = x[["skewnessType"]]() == "Negative Skew",
           errormsg = paste(x$short_name,"is not negative skew"))

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
makeChecks(assertionName =  "PositiveSkew",
           cond = x[["skewnessType"]]() == "Positive Skew",
           errormsg = paste(x$short_name,"is not positive skew"))

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
makeChecks(assertionName =  "NoSkew",
           cond = x[["skewnessType"]]() == "No Skew",
           errormsg = paste(x$short_name,"is not no skew"))

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
makeChecks(assertionName =  "Kurtosis",
           cond = x[["kurtosisType"]]() == type,
           errormsg = paste(x$short_name,"is not",type),
           args = alist(x=, type=))

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
makeChecks(assertionName =  "Platykurtic",
           cond = x[["kurtosisType"]]() == "platykurtic",
           errormsg = paste(x$short_name,"is not platykurtic"))

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
makeChecks(assertionName =  "Mesokurtic",
           cond = x[["kurtosisType"]]() == "mesokurtic",
           errormsg = paste(x$short_name,"is not mesokurtic"))

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
makeChecks(assertionName =  "Leptokurtic",
           cond = x[["kurtosisType"]]() == "leptokurtic",
           errormsg = paste(x$short_name,"is not leptokurtic"))