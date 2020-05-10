
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name WeightedDiscrete
#' @template SDist
#' @templateVar ClassName WeightedDiscrete
#' @templateVar DistName WeightedDiscrete
#' @templateVar uses in empirical estimators such as Kaplan-Meier
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the x values and the pdf as the probabilities. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar constructor data
#' @templateVar arg1 \code{data} \tab data.frame \tab matrix-style object of observations and probabilities. See details. \cr
#' @templateVar constructorDets an object that can be coerced to a data.frame containing columns 'sample' and at least one of 'pdf' and 'cdf', see examples.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function and \code{\link{Empirical}} for the closely related Empirical distribution.
#'
#' @examples
#' x = WeightedDiscrete$new(data = data.frame(x = 1:3, pdf = c(1/5, 3/5, 1/5)))
#' WeightedDiscrete$new(data = data.frame(x = 1:3, cdf = c(1/5, 4/5, 1))) # equivalently
#'
#' # d/p/q/r
#' x$pdf(1:5)
#' x$cdf(1:5) # Assumes ordered in construction
#' x$quantile(0.42) # Assumes ordered in construction
#' x$rand(10)
#'
#' # Statistics
#' x$mean()
#' x$variance()
#'
#' summary(x)
#'
#' @export
NULL
#-------------------------------------------------------------
# WeightedDiscrete Distribution Definition
#-------------------------------------------------------------
WeightedDiscrete <- R6Class("WeightedDiscrete", inherit = SDistribution, lock_objects = F)
WeightedDiscrete$set("public","name","WeightedDiscrete")
WeightedDiscrete$set("public","short_name","WeightDisc")
WeightedDiscrete$set("public","description","WeightedDiscrete Probability Distribution.")

WeightedDiscrete$set("public","mode",function(which = "all"){
  x = self$getParameterValue("data")
  pdf = self$getParameterValue("pdf")

  if(which == "all") {
    return(x[pdf == max(pdf)])
  } else {
    return(x[pdf == max(pdf)][which])
  }
})
WeightedDiscrete$set("public","mean",function(){
  return(sum(self$getParameterValue("data") * self$getParameterValue("pdf")))
})
WeightedDiscrete$set("public","variance",function(){
  return(sum((self$getParameterValue("data") - self$mean())^2 * self$getParameterValue("pdf")))
})
WeightedDiscrete$set("public","skewness",function(){
  return(sum(((self$getParameterValue("data") - self$mean())/self$stdev())^3 * self$getParameterValue("pdf")))
})
WeightedDiscrete$set("public","kurtosis",function(excess = TRUE){
  kurt = sum(((self$getParameterValue("data") - self$mean())/self$stdev())^4 * self$getParameterValue("pdf"))
  if(excess)
    return(kurt - 3)
  else
    return(kurt)
})
WeightedDiscrete$set("public","entropy",function(base = 2){
  pdf = self$getParameterValue("pdf")
  return(-sum(pdf * log(pdf, base)))
})
WeightedDiscrete$set("public","mgf",function(t){
  x = self$getParameterValue("data")
  pdf = self$getParameterValue("pdf")

  if(length(t) == 1)
    return(sum(exp(x*t) * (pdf)))
  else{
    nr = length(t)
    nc = length(x)
    return(as.numeric(
      exp(matrix(x, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(pdf, nrow = nc, ncol = 1)
    ))
  }
})
WeightedDiscrete$set("public","cf",function(t){
  x = self$getParameterValue("data")
  pdf = self$getParameterValue("pdf")

  if(length(t) == 1)
    return(sum(exp(x*t*1i) * (pdf)))
  else{
    nr = length(t)
    nc = length(x)
    return(as.complex(
      exp(matrix(x*1i, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(pdf, nrow = nc, ncol = 1)
    ))
  }
})
WeightedDiscrete$set("public","pgf",function(z){
  x = self$getParameterValue("data")
  pdf = self$getParameterValue("pdf")

  if(length(z) == 1)
    return(sum((z^x) * pdf))
  else{
    nr = length(z)
    nc = length(x)
    return(as.numeric(
      (matrix(z, nrow = nr, ncol = nc) ^ matrix(x, nrow = nr, ncol = nc, byrow = z)) %*%
        matrix(pdf, nrow = nc, ncol = 1)
    ))
  }
})
WeightedDiscrete$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  message("WeightedDiscrete cannot be modified after construction.")
  return(NULL)
})

WeightedDiscrete$set("public","getParameterValue",function(id, error = "warn"){
  if("data" %in% id)
    return(super$getParameterValue("data", error))
  else if("pdf" %in% id)
    return(super$getParameterValue("pdf", error))
  else if("cdf" %in% id)
    return(super$getParameterValue("cdf", error))
})

WeightedDiscrete$set("private",".pdf",function(x, log = FALSE){
  pdf = self$getParameterValue("pdf")
  if (checkmate::testList(pdf)) {
    pdf = matrix(unlist(pdf), ncol = length(pdf))
    data = matrix(unlist(self$getParameterValue("data")), ncol = ncol(pdf))
    return(C_Vec_WeightedDiscretePdf(x, data, pdf, log))
  } else {
    return(C_WeightedDiscretePdf(x, self$getParameterValue("data"), pdf, log))
  }
})
WeightedDiscrete$set("private",".cdf",function(x, lower.tail = TRUE, log.p = FALSE){
  cdf = self$getParameterValue("cdf")
  if (checkmate::testList(cdf)) {
    if(is.null(cdf[[1]])) {
      stop("'cdf' must be supplied in VectorDistribution constructor")
    }
    cdf = matrix(unlist(cdf), ncol = length(cdf))
    data = matrix(unlist(self$getParameterValue("data")), ncol = ncol(cdf))
    return(C_Vec_WeightedDiscreteCdf(x, data, cdf, lower.tail, log.p))
  } else {
    return(C_WeightedDiscreteCdf(x, self$getParameterValue("data"), cdf, lower.tail, log.p))
  }
})
WeightedDiscrete$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  cdf = self$getParameterValue("cdf")
  if (checkmate::testList(cdf)) {
    if(is.null(cdf[[1]])) {
      stop("'cdf' must be supplied in VectorDistribution constructor")
    }
    cdf = matrix(unlist(cdf), ncol = length(cdf))
    data = matrix(unlist(self$getParameterValue("data")), ncol = ncol(cdf))
    return(C_Vec_WeightedDiscreteQuantile(x, data, cdf, lower.tail, log.p))
  } else {
    return(C_WeightedDiscreteQuantile(x, self$getParameterValue("data"), cdf, lower.tail, log.p))
  }
})
WeightedDiscrete$set("private",".rand",function(n){
  pdf = self$getParameterValue("pdf")
  if (checkmate::testList(pdf)) {
    data = self$getParameterValue("data")
    rand = matrix(nrow = n, ncol = length(pdf))
    for (i in seq_along(pdf)) {
      rand[,i] = sample(data[[i]], n, TRUE, pdf[[i]])
    }
  } else {
    rand = sample(self$getParameterValue("data"), n, TRUE, pdf)
  }
  return(rand)
})
WeightedDiscrete$set("private", ".log", TRUE)
WeightedDiscrete$set("private", ".traits", list(valueSupport = "discrete", variateForm = "univariate"))

WeightedDiscrete$set("private",".data", "Deprecated - use self$getParameterValue instead.")
WeightedDiscrete$set("private",".getRefParams", function(paramlst){
  return(paramlst)
})

WeightedDiscrete$set("public","initialize",function(data, pdf = NULL, cdf = NULL, decorators = NULL, verbose = FALSE){

  if(class(data)[1] %in% c("data.frame", "data.table", "matrix")) {
    warning("'data' argument should now be given as a vector, with other arguments passed to
            'pdf' and 'cdf'. In the next release this will throw an error if not changed.")
    pdf = data$pdf
    cdf = data$cdf
    data = data$data
  }

  checkmate::assertNumeric(pdf, lower = 0, upper = 1, .var.name = "pdf is not valid", null.ok = TRUE)
  checkmate::assertNumeric(cdf, lower = 0, upper = 1, .var.name = "cdf is not valid", null.ok = TRUE)

  if(!is.null(pdf) & is.null(cdf)){
    cdf = cumsum(pdf)
  } else if(!is.null(cdf) & is.null(pdf)){
    pdf = c(cdf[1], diff(cdf))
  } else if(is.null(pdf) & is.null(cdf)) {
    stop("At least one of 'pdf' and 'cdf' should be supplied.")
  }

  # if (sum(pdf) != 1) {
  #   stop("'pdf' should sum to 1")
  # }

  private$.parameters = getParameterSet(self, data, pdf, cdf)
  super$setParameterValue(lst = list(data = data, pdf = pdf, cdf = cdf))

  super$initialize(decorators = decorators,
                   support = Set$new(data, class = "numeric"),
                   type = Reals$new())
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "WeightDisc", ClassName = "WeightedDiscrete",
                                                     Type = "\u211D", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "-"))
