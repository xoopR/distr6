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
liesInSupport <- function(distribution, x){}
liesInType <- function(distribution, x){}

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
  if(distribution[[accessor]] == feature)
    return(TRUE)
  else
    return(FALSE)
}
isDistributionFeature <- function(distribution, accessor, feature){
  testDistributionFeature(distribution, accessor, feature)
}

assertVariateForm <- function(distribution, type){
  assertDistributionFeature(distribution, variateForm, type)
}
checkVariateForm <- function(distribution, type){
  checkDistributionFeature(distribution, variateForm, type)
}
testVariateForm <- function(distribution, type){
  testDistributionFeature(distribution, variateForm, type)
}
isVariateForm <- function(distribution, type){
  isDistributionFeature(distribution, variateForm, type)
}

assertUnivariate <- function(distribution){
  assertVariateForm(distribution, "Univariate")
}
checkUnivariate <- function(distribution){
  checkVariateForm(distribution, "Univariate")
}
testUnivariate <- function(distribution){
  testVariateForm(distribution, "Univariate")
}
isUnivariate <- function(distribution){
  isVariateForm(distribution, "Univariate")
}

assertMultivariate <- function(distribution){
  assertVariateForm(distribution, "Multivariate")
}
checkMultivariate <- function(distribution){
  checkVariateForm(distribution, "Multivariate")
}
testMultivariate <- function(distribution){
  testVariateForm(distribution, "Multivariate")
}
isMultivariate <- function(distribution){
  isVariateForm(distribution, "Multivariate")
}

assertMatrixvariate <- function(distribution){
  assertVariateForm(distribution, "Matrixvariate")
}
checkMatrixvariate <- function(distribution){
  checkVariateForm(distribution, "Matrixvariate")
}
testMatrixvariate <- function(distribution){
  testVariateForm(distribution, "Matrixvariate")
}
isMatrixvariate <- function(distribution){
  isVariateForm(distribution, "Matrixvariate")
}

assertValueSupport <- function(distribution, type){
  assertDistributionFeature(distribution, valueSupport, type)
}
checkValueSupport <- function(distribution, type){
  checkDistributionFeature(distribution, valueSupport, type)
}
testValueSupport <- function(distribution, type){
  testDistributionFeature(distribution, valueSupport, type)
}
isValueSupport <- function(distribution, type){
  isDistributionFeature(distribution, valueSupport, type)
}

assertContinuous <- function(distribution){
  assertValueSupport(distribution, "Continuous")
}
checkContinuous <- function(distribution){
  checkValueSupport(distribution, "Continuous")
}
testContinuous <- function(distribution){
  testValueSupport(distribution, "Continuous")
}
isContinuous <- function(distribution){
  isValueSupport(distribution, "Continuous")
}

assertDiscrete <- function(distribution){
  assertValueSupport(distribution, "Discrete")
}
checkDiscrete <- function(distribution){
  checkValueSupport(distribution, "Discrete")
}
testDiscrete <- function(distribution){
  testValueSupport(distribution, "Discrete")
}
isDiscrete <- function(distribution){
  isValueSupport(distribution, "Discrete")
}

assertMixture <- function(distribution){
  assertValueSupport(distribution, "Mixture")
}
checkMixture <- function(distribution){
  checkValueSupport(distribution, "Mixture")
}
testMixture <- function(distribution){
  testValueSupport(distribution, "Mixture")
}
isMixture <- function(distribution){
  isValueSupport(distribution, "Mixture")
}

assertSymmetric <- function(distribution){
  assertDistributionFeature(distribution, symmetry, "Symmetric")
}
checkSymmetric <- function(distribution){
  checkDistributionFeature(distribution, symmetry, "Symmetric")
}
testSymmetric <- function(distribution){
  testDistributionFeature(distribution, symmetry, "Symmetric")
}
isSymmetric <- function(distribution){
  isDistributionFeature(distribution, symmetry, "Symmetric")
}

assertSkewness <- function(distribution, feature){
  assertDistributionFeature(distribution, skewnessType, feature)
}
checkSkewness <- function(distribution, feature){
  checkDistributionFeature(distribution, skewnessType, feature)
}
testSkewness <- function(distribution, feature){
  testDistributionFeature(distribution, skewnessType, feature)
}
isSkewness <- function(distribution, feature){
  isDistributionFeature(distribution, skewnessType, feature)
}

assertNegativeSkew <- function(distribution){
  assertSkewness(distribution, "Negative Skew")
}
checkNegativeSkew <- function(distribution){
  checkSkewness(distribution, "Negative Skew")
}
testNegativeSkew <- function(distribution){
  testSkewness(distribution, "Negative Skew")
}
isNegativeSkew <- function(distribution){
  isSkewness(distribution, "Negative Skew")
}

assertPositiveSkew <- function(distribution){
  assertSkewness(distribution, "Positive Skew")
}
checkPositiveSkew <- function(distribution){
  checkSkewness(distribution, "Positive Skew")
}
testPositiveSkew <- function(distribution){
  testSkewness(distribution, "Positive Skew")
}
isPositiveSkew <- function(distribution){
  isSkewness(distribution, "Positive Skew")
}

assertNoSkew <- function(distribution){
  assertSkewness(distribution, "No Skew")
}
checkNoSkew <- function(distribution){
  checkSkewness(distribution, "No Skew")
}
testNoSkew <- function(distribution){
  testSkewness(distribution, "No Skew")
}
isNoSkew <- function(distribution){
  isSkewness(distribution, "No Skew")
}


assertKurtosis <- function(distribution, feature){
  assertDistributionFeature(distribution, kurtosisType, feature)
}
checkKurtosis <- function(distribution, feature){
  checkDistributionFeature(distribution, kurtosisType, feature)
}
testKurtosis <- function(distribution, feature){
  testDistributionFeature(distribution, kurtosisType, feature)
}
isKurtosis <- function(distribution, feature){
  isDistributionFeature(distribution, kurtosisType, feature)
}

assertPlatykurtic <- function(distribution){
  assertKurtosis(distribution, "Platykurtic")
}
checkPlatykurtic <- function(distribution){
  checkKurtosis(distribution, "Platykurtic")
}
testPlatykurtic <- function(distribution){
  testKurtosis(distribution, "Platykurtic")
}
isPlatykurtic <- function(distribution){
  testKurtosis(distribution, "Platykurtic")
}

assertMesokurtic <- function(distribution){
  assertKurtosis(distribution, "Mesokurtic")
}
checkMesokurtic <- function(distribution){
  checkKurtosis(distribution, "Mesokurtic")
}
testMesokurtic <- function(distribution){
  testKurtosis(distribution, "Mesokurtic")
}
isMesokurtic <- function(distribution){
  testKurtosis(distribution, "Mesokurtic")
}

assertLeptokurtic <- function(distribution){
  assertKurtosis(distribution, "Leptokurtic")
}
checkLeptokurtic <- function(distribution){
  checkKurtosis(distribution, "Leptokurtic")
}
testLeptokurtic <- function(distribution){
  testKurtosis(distribution, "Leptokurtic")
}
isLeptokurtic <- function(distribution){
  testKurtosis(distribution, "Leptokurtic")
}