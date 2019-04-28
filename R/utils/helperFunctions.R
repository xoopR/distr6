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
addition <- function(distribution1, distribution2,
                     name = "CustomDistribution"){
  assertDistributionList(list(distribution1, distribution2))

  paramset <- rbind(distribution1$parameters(), distribution2$parameters())
  paramset$id[duplicated(paramset$id)]

  distr = Distribution$new(pdf = distribution1$convolution(distribution2), name,
                           parameters = distribution1$parameters)
  formals(distr$.__enclos_env__$private$.pdf)$self = distr
  return(distr)
}
subtraction <- function(...){}
multiplication <- function(...){}
division <- function(...){}
exponentiation <- function(...){}

exkurtosisType <- function(kurtosis){
  if(kurtosis < 0)
    return("platykurtic")
  else if(kurtosis == 0)
    return("mesokurtic")
  else
    return("leptokurtic")
}
skewType <- function(skew){
  if(skew < 0)
    return("negative skew")
  else if(skew == 0)
    return("no skew")
  else
    return("positive skew")
}

# Checkmate template as follows:
#  test - TRUE/FALSE
#  is - TRUE/FALSE
#  assert - invisible(x)/stop
#  check - TRUE/message
assertThat <- function(x, cond, errormsg){
  if(cond)
    invisible(x)
  else
    stop(errormsg)
}
checkThat <- function(cond, errormsg){
  if(cond)
    return(TRUE)
  else
    return(errormsg)
}
testThat <- function(cond){
  if(cond)
    return(TRUE)
  else
    return(FALSE)
}
isThat <- function(cond){
  return(testThat(cond))
}

# Auto-generation of validation checks
makeChecks <- function(assertionName, cond, errormsg, args = alist(x=)){
  cond = substitute(cond)
  errormsg = substitute(errormsg)
  value = function(x){}
  formals(value) = args
  body(value) = substitute(assertThat(x,arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("assert",assertionName), value = value,
         pos = parent.env(environment()))

  body(value) = substitute(checkThat(arg1,arg2),list(arg1=cond,arg2=errormsg))
  assign(paste0("check",assertionName), value = value,
         pos = parent.env(environment()))

  body(value) = substitute(testThat(arg1),list(arg1=cond))
  assign(paste0("test",assertionName), value = value,
         pos = parent.env(environment()))

  body(value) = substitute(isThat(arg1),list(arg1=cond))
  assign(paste0("is",assertionName), value = value,
         pos = parent.env(environment()))
}

# assertDistribution
makeChecks(assertionName = "Distribution",
           cond = inherits(x,"Distribution"),
           errormsg = paste(x,"is not an R6 Distribution object"))

# assertDistributionList
makeChecks(assertionName =  "DistributionList",
           cond = all(unlist(lapply(x,inherits,"Distribution"))),
           errormsg = "One or more items in the list are not Distributions")

# assertDistributionFeature
makeChecks(assertionName =  "DistributionFeature",
           cond = x[[accessor]]() == feature,
           errormsg = paste(x$short_name(),"is not",feature),
           args = alist(x=, accessor=, feature=))

# assertVariateForm
makeChecks(assertionName =  "VariateForm",
           cond = x[["variateForm"]]() == type,
           errormsg = paste(x$short_name(),"is not",type),
           args = alist(x=, type=))

# assertUnivariate
makeChecks(assertionName =  "Univariate",
           cond = x[["variateForm"]]() == "univariate",
           errormsg = paste(x$short_name(),"is not univariate"))

# assertMultivariate
makeChecks(assertionName =  "Multivariate",
           cond = x[["variateForm"]]() == "multivariate",
           errormsg = paste(x$short_name(),"is not multivariate"))

# assertMatrixvariate
makeChecks(assertionName =  "Matrixvariate",
           cond = x[["variateForm"]]() == "matrixvariate",
           errormsg = paste(x$short_name(),"is not matrixvariate"))

# assertValueSupport
makeChecks(assertionName =  "ValueSupport",
           cond = x[["valueSupport"]]() == type,
           errormsg = paste(x$short_name(),"is not",type),
           args = alist(x=, type=))

# assertContinuous
makeChecks(assertionName =  "Continuous",
           cond = x[["valueSupport"]]() == "continuous",
           errormsg = paste(x$short_name(),"is not continuous"))

# assertDiscrete
makeChecks(assertionName =  "Discrete",
           cond = x[["valueSupport"]]() == "discrete",
           errormsg = paste(x$short_name(),"is not discrete"))

# assertMixture
makeChecks(assertionName =  "Mixture",
           cond = x[["valueSupport"]]() == "mixture",
           errormsg = paste(x$short_name(),"is not mixture"))

# assertSymmetric
makeChecks(assertionName =  "Symmetric",
           cond = x[["symmetry"]](),
           errormsg = paste(x$short_name(),"is not symmetric"))

# assertSkewness
makeChecks(assertionName =  "Skewness",
           cond = x[["skewnessType"]]() == type,
           errormsg = paste(x$short_name(),"is not",type),
           args = alist(x=, type=))

# assertNegativeSkew
makeChecks(assertionName =  "NegativeSkew",
           cond = x[["skewnessType"]]() == "Negative Skew",
           errormsg = paste(x$short_name(),"is not negative skew"))

# assertPositiveSkew
makeChecks(assertionName =  "PositiveSkew",
           cond = x[["skewnessType"]]() == "Positive Skew",
           errormsg = paste(x$short_name(),"is not positive skew"))

# assertNoSkew
makeChecks(assertionName =  "NoSkew",
           cond = x[["skewnessType"]]() == "No Skew",
           errormsg = paste(x$short_name(),"is not no skew"))

# assertKurtosis
makeChecks(assertionName =  "Kurtosis",
           cond = x[["kurtosisType"]]() == type,
           errormsg = paste(x$short_name(),"is not",type),
           args = alist(x=, type=))

# assertPlatykurtic
makeChecks(assertionName =  "Platykurtic",
           cond = x[["kurtosisType"]]() == "platykurtic",
           errormsg = paste(x$short_name(),"is not platykurtic"))

# assertMesokurtic
makeChecks(assertionName =  "Mesokurtic",
           cond = x[["kurtosisType"]]() == "mesokurtic",
           errormsg = paste(x$short_name(),"is not mesokurtic"))

# assertLeptokurtic
makeChecks(assertionName =  "Leptokurtic",
           cond = x[["kurtosisType"]]() == "leptokurtic",
           errormsg = paste(x$short_name(),"is not leptokurtic"))