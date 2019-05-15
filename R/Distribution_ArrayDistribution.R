ArrayDistribution <- R6::R6Class("ArrayDistribution", inherit = Distribution)
ArrayDistribution$set("public","initialize",function(distribution, paramVector){
  distribution = paste0(substitute(distribution))
  if(!(distribution %in% listDistributions(simplify = T)))
    stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

  distribution = get(distribution)$new()
  param_filter = unlist(distribution$parameters(as.df = T)["settable"])
  ids = distribution$parameters(as.df = T)[param_filter,"id"]
  lowers = distribution$parameters(as.df = T)[param_filter,"lower"]
  uppers = distribution$parameters(as.df = T)[param_filter,"upper"]
  classes = distribution$parameters(as.df = T)[param_filter,"class"]

  params = sapply(paramVector, function(x){
    checkmate::assert(length(x)==length(ids))
    for(i in 1:length(x)){
      index = which(ids %in% names(x)[[i]])
      x[[i]] = as(x[[i]], classes[index])
      checkmate::assertNumber(x[[i]], lower = lowers[index], upper = uppers[index])
    }
    return(x)
  })
  rownames(params) = 1:nrow(params)

})
