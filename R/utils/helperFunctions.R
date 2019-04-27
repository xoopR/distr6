listDistributions <- function(simplify=FALSE,traits=NULL){
  y = sapply(ls(name=".GlobalEnv"),function(x){
    if(inherits(get(x),"R6ClassGenerator")){
      if(environmentName(get(x)$get_inherit()) == "Distribution_generator")
        return(get(x)$classname)
      else
        return(FALSE)
    } else
      return(FALSE)
  })
  y = (y[y!="FALSE"])
  if(simplify)
    return(y)
  else{
    distrs = do.call(rbind,lapply(y, function(x){
      x = get(x)
      ClassName = x$classname
      x = x$new()
      ShortName = x$short.name
      traits = as.data.frame(x$getTraits())
      return(cbind(ShortName,ClassName,traits))
    }))
    row.names(distrs) = NULL
    if(!is.null(traits)){
      traits = as.list(traits)
      for(i in 1:length(traits))
        distrs = distrs[distrs[,colnames(distrs) %in% names(traits)[[i]]] == traits[[i]],]
    }
    return(distrs)
  }
}

strprint.list <- function(x,...){
  lapply(x,strprint,...)
}

getR6Class <- function(x){
  return(get(class(x)[[1]])$classname)
}

# To do
liesInDistrDomain <- function(distribution, x){}
liesInType <- function(distribution, x){}
addition <- function(...){
  dots = list(...)
  assertDistributionList(dots)
}
subtraction <- function(...){}
multiplication <- function(...){}
division <- function(...){}
exponentiation <- function(...){}


# Checkmate template as follows:
#  test - TRUE/FALSE
#  is - TRUE/FALSE
#  assert - invisible(x)/stop
#  check - TRUE/message

assertDistributionList <- function(list){
  cond = all(unlist(lapply(list,inherits,"Distribution")))
  if(cond)
    invisible(list)
  else
    stop("One or more items in the list are not Distributions")
}
checkDistributionList <- function(list){
  cond = all(unlist(lapply(list,inherits,"Distribution")))
  if(cond)
    return(TRUE)
  else
    return("One or more items in the list are not Distributions")
}
testDistributionList <- function(list){
  cond = all(unlist(lapply(list,inherits,"Distribution")))
  if(cond)
    return(TRUE)
  else
    return(FALSE)
}
isDistributionList <- function(list){
  testDistributionList(list)
}

assertDistributionFeature <- function(distribution, accessor, feature){
  if(distribution[[accessor]] == feature)
    invisible(distribution)
  else
    stop(paste("Distribution is not",feature))
}
checkDistributionFeature <- function(distribution, accessor, feature){
  if(distribution[[accessor]] == feature)
    return(TRUE)
  else
    return(paste("Distribution is not",feature))
}
testDistributionFeature <- function(distribution, accessor, feature){
  if(distribution[[accessor]]() == feature)
    return(TRUE)
  else
    return(FALSE)
}
isDistributionFeature <- function(distribution, accessor, feature){
  testDistributionFeature(distribution, accessor, feature)
}

assertVariateForm <- function(distribution, type){
  assertDistributionFeature(distribution, "variateForm", type)
}
checkVariateForm <- function(distribution, type){
  checkDistributionFeature(distribution, "variateForm", type)
}
testVariateForm <- function(distribution, type){
  testDistributionFeature(distribution, "variateForm", type)
}
isVariateForm <- function(distribution, type){
  isDistributionFeature(distribution, "variateForm", type)
}

assertUnivariate <- function(distribution){
  assertVariateForm(distribution, "univariate")
}
checkUnivariate <- function(distribution){
  checkVariateForm(distribution, "univariate")
}
testUnivariate <- function(distribution){
  testVariateForm(distribution, "univariate")
}
isUnivariate <- function(distribution){
  isVariateForm(distribution, "univariate")
}

assertMultivariate <- function(distribution){
  assertVariateForm(distribution, "multivariate")
}
checkMultivariate <- function(distribution){
  checkVariateForm(distribution, "multivariate")
}
testMultivariate <- function(distribution){
  testVariateForm(distribution, "multivariate")
}
isMultivariate <- function(distribution){
  isVariateForm(distribution, "multivariate")
}

assertMatrixvariate <- function(distribution){
  assertVariateForm(distribution, "matrixvariate")
}
checkMatrixvariate <- function(distribution){
  checkVariateForm(distribution, "matrixvariate")
}
testMatrixvariate <- function(distribution){
  testVariateForm(distribution, "matrixvariate")
}
isMatrixvariate <- function(distribution){
  isVariateForm(distribution, "matrixvariate")
}

assertValueSupport <- function(distribution, type){
  assertDistributionFeature(distribution, "valueSupport", type)
}
checkValueSupport <- function(distribution, type){
  checkDistributionFeature(distribution, "valueSupport", type)
}
testValueSupport <- function(distribution, type){
  testDistributionFeature(distribution, "valueSupport", type)
}
isValueSupport <- function(distribution, type){
  isDistributionFeature(distribution, "valueSupport", type)
}

assertContinuous <- function(distribution){
  assertValueSupport(distribution, "continuous")
}
checkContinuous <- function(distribution){
  checkValueSupport(distribution, "continuous")
}
testContinuous <- function(distribution){
  testValueSupport(distribution, "continuous")
}
isContinuous <- function(distribution){
  isValueSupport(distribution, "continuous")
}

assertDiscrete <- function(distribution){
  assertValueSupport(distribution, "discrete")
}
checkDiscrete <- function(distribution){
  checkValueSupport(distribution, "discrete")
}
testDiscrete <- function(distribution){
  testValueSupport(distribution, "discrete")
}
isDiscrete <- function(distribution){
  isValueSupport(distribution, "discrete")
}

assertMixture <- function(distribution){
  assertValueSupport(distribution, "mixture")
}
checkMixture <- function(distribution){
  checkValueSupport(distribution, "mixture")
}
testMixture <- function(distribution){
  testValueSupport(distribution, "mixture")
}
isMixture <- function(distribution){
  isValueSupport(distribution, "mixture")
}

assertSymmetric <- function(distribution){
  assertDistributionFeature(distribution, "symmetry", "Symmetric")
}
checkSymmetric <- function(distribution){
  checkDistributionFeature(distribution, "symmetry", "Symmetric")
}
testSymmetric <- function(distribution){
  testDistributionFeature(distribution, "symmetry", "Symmetric")
}
isSymmetric <- function(distribution){
  isDistributionFeature(distribution, "symmetry", "Symmetric")
}

assertSkewness <- function(distribution, feature){
  assertDistributionFeature(distribution, "skewnessType", feature)
}
checkSkewness <- function(distribution, feature){
  checkDistributionFeature(distribution, "skewnessType", feature)
}
testSkewness <- function(distribution, feature){
  testDistributionFeature(distribution, "skewnessType", feature)
}
isSkewness <- function(distribution, feature){
  isDistributionFeature(distribution, "skewnessType", feature)
}

assertNegativeSkew <- function(distribution){
  assertSkewness(distribution, "negative skew")
}
checkNegativeSkew <- function(distribution){
  checkSkewness(distribution, "negative skew")
}
testNegativeSkew <- function(distribution){
  testSkewness(distribution, "negative skew")
}
isNegativeSkew <- function(distribution){
  isSkewness(distribution, "negative skew")
}

assertPositiveSkew <- function(distribution){
  assertSkewness(distribution, "positive skew")
}
checkPositiveSkew <- function(distribution){
  checkSkewness(distribution, "positive skew")
}
testPositiveSkew <- function(distribution){
  testSkewness(distribution, "positive skew")
}
isPositiveSkew <- function(distribution){
  isSkewness(distribution, "positive skew")
}

assertNoSkew <- function(distribution){
  assertSkewness(distribution, "no skew")
}
checkNoSkew <- function(distribution){
  checkSkewness(distribution, "no skew")
}
testNoSkew <- function(distribution){
  testSkewness(distribution, "no skew")
}
isNoSkew <- function(distribution){
  isSkewness(distribution, "no skew")
}


assertKurtosis <- function(distribution, feature){
  assertDistributionFeature(distribution, "kurtosisType", feature)
}
checkKurtosis <- function(distribution, feature){
  checkDistributionFeature(distribution, "kurtosisType", feature)
}
testKurtosis <- function(distribution, feature){
  testDistributionFeature(distribution, "kurtosisType", feature)
}
isKurtosis <- function(distribution, feature){
  isDistributionFeature(distribution, "kurtosisType", feature)
}

assertPlatykurtic <- function(distribution){
  assertKurtosis(distribution, "platykurtic")
}
checkPlatykurtic <- function(distribution){
  checkKurtosis(distribution, "platykurtic")
}
testPlatykurtic <- function(distribution){
  testKurtosis(distribution, "platykurtic")
}
isPlatykurtic <- function(distribution){
  testKurtosis(distribution, "platykurtic")
}

assertMesokurtic <- function(distribution){
  assertKurtosis(distribution, "mesokurtic")
}
checkMesokurtic <- function(distribution){
  checkKurtosis(distribution, "mesokurtic")
}
testMesokurtic <- function(distribution){
  testKurtosis(distribution, "mesokurtic")
}
isMesokurtic <- function(distribution){
  testKurtosis(distribution, "mesokurtic")
}

assertLeptokurtic <- function(distribution){
  assertKurtosis(distribution, "leptokurtic")
}
checkLeptokurtic <- function(distribution){
  checkKurtosis(distribution, "leptokurtic")
}
testLeptokurtic <- function(distribution){
  testKurtosis(distribution, "leptokurtic")
}
isLeptokurtic <- function(distribution){
  testKurtosis(distribution, "leptokurtic")
}