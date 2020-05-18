
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
  data = self$getParameterValue("data")

  if(which == "all") {
    return(data$x[data$pdf == max(data$pdf)])
  } else {
    return(data$x[data$pdf == max(data$pdf)][which])
  }
})
WeightedDiscrete$set("public","mean",function(){
  data = self$getParameterValue("data")
  return(sum(data$x * data$pdf))
})
WeightedDiscrete$set("public","variance",function(){
  data = self$getParameterValue("data")
  return(sum((data$x - self$mean())^2 * data$pdf))
})
WeightedDiscrete$set("public","skewness",function(){
  data = self$getParameterValue("data")
  return(sum(((data$x - self$mean())/self$stdev())^3 * data$pdf))
})
WeightedDiscrete$set("public","kurtosis",function(excess = TRUE){
  data = self$getParameterValue("data")
  kurt = sum(((data$x - self$mean())/self$stdev())^4 * data$pdf)
  if(excess)
    return(kurt - 3)
  else
    return(kurt)
})
WeightedDiscrete$set("public","entropy",function(base = 2){
  pdf = self$getParameterValue("data")$pdf
  return(-sum(pdf * log(pdf, base)))
})
WeightedDiscrete$set("public","mgf",function(t){
  data = self$getParameterValue("data")

  if(length(t) == 1)
    return(sum(exp(data$x*t) * (data$pdf)))
  else{
    nr = length(t)
    nc = length(data$x)
    return(as.numeric(
      exp(matrix(data$x, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$pdf, nrow = nc, ncol = 1)
    ))
  }
})
WeightedDiscrete$set("public","cf",function(t){
  data = self$getParameterValue("data")

  if(length(t) == 1)
    return(sum(exp(data$x*t*1i) * (data$pdf)))
  else{
    nr = length(t)
    nc = length(data$x)
    return(as.complex(
      exp(matrix(data$x * 1i, nrow = nr, ncol = nc, byrow = T) *
            matrix(t, nrow = nr, ncol = nc)) %*% matrix(data$pdf, nrow = nc, ncol = 1)
    ))
  }
})
WeightedDiscrete$set("public","pgf",function(z){
  data = self$getParameterValue("data")

  if(length(z) == 1)
    return(sum((z^data$x) * data$pdf))
  else{
    nr = length(z)
    nc = length(data$x)
    return(as.numeric(
      (matrix(z, nrow = nr, ncol = nc) ^ matrix(data$x, nrow = nr, ncol = nc, byrow = z)) %*%
        matrix(data$pdf, nrow = nc, ncol = 1)
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
})

WeightedDiscrete$set("private",".pdf",function(x, log = FALSE){
  data = self$getParameterValue("data")

  if (checkmate::testList(data)) {
    data = unlist(data)
    pdf = matrix(as.numeric(data[names(data)=="pdf"]), ncol = nrow(data))
    data = matrix(as.numeric(data[names(data)=="x"]), ncol = ncol(pdf))
    return(C_Vec_WeightedDiscretePdf(x, data$x, data$pdf, log))
  } else {
    return(C_WeightedDiscretePdf(x, data, pdf, log))
  }
})
WeightedDiscrete$set("private",".cdf",function(x, lower.tail = TRUE, log.p = FALSE){
  data = self$getParameterValue("data")

  if (checkmate::testList(data)) {
    cdf = matrix(as.numeric(data[names(data)=="cdf"]), ncol = nrow(data))
    data = matrix(as.numeric(data[names(data)=="x"]), ncol = ncol(cdf))
    return(C_Vec_WeightedDiscreteCdf(x, data, cdf, lower.tail, log.p))
  } else {
    return(C_WeightedDiscreteCdf(x, data$x, data$cdf, lower.tail, log.p))
  }
})
WeightedDiscrete$set("private",".quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  data = self$getParameterValue("data")

  if (checkmate::testList(data)) {
    cdf = matrix(as.numeric(data[names(data)=="cdf"]), ncol = nrow(data))
    data = matrix(as.numeric(data[names(data)=="x"]), ncol = ncol(cdf))
    return(C_Vec_WeightedDiscreteQuantile(x, data, cdf, lower.tail, log.p))
  } else {
    return(C_WeightedDiscreteQuantile(x, data$x, data$cdf, lower.tail, log.p))
  }
})
WeightedDiscrete$set("private",".rand",function(n){
  data = self$getParameterValue("data")

  if (checkmate::testList(data)) {
    rand = matrix(nrow = n, ncol = nrow(pdf))
    for (i in seq_along(data)) {
      rand[,i] = sample(data[[i]]$x, n, TRUE, data[[i]]$pdf)
    }
  } else {
    rand = sample(data$x, n, TRUE, data$pdf)
  }
  return(rand)
})
WeightedDiscrete$set("private", ".log", TRUE)
WeightedDiscrete$set("private", ".traits", list(valueSupport = "discrete", variateForm = "univariate"))

WeightedDiscrete$set("private",".data", "Deprecated - use self$getParameterValue instead.")
WeightedDiscrete$set("private",".getRefParams", function(paramlst){
  return(paramlst)
})

WeightedDiscrete$set("public","initialize",function(data, decorators = NULL, verbose = FALSE){

  data <- data.table::as.data.table(data)
  checkmate::assert(all(colnames(data) %in% c("pdf","cdf","x")),
                    .var.name = "data column names should be one of 'pdf', 'cdf', 'x")
  checkmate::assert("x" %in% colnames(data),
                    .var.name = "'x' must be included in data column names")
  checkmate::assert(any(c("pdf","cdf") %in% colnames(data)),
                    .var.name = "at least one of 'pdf' and 'cdf' must be included in data column names")

  if("pdf" %in% colnames(data) & !("cdf" %in% colnames(data))){
    data$cdf = cumsum(data$pdf)
  } else if("cdf" %in% colnames(data) & !("pdf" %in% colnames(data))){
    data$pdf = c(data$cdf[1], diff(data$cdf))
  }

  checkmate::assertNumeric(data$pdf, lower = 0, upper = 1, .var.name = "pdf is not valid")
  checkmate::assertNumeric(data$cdf, lower = 0, upper = 1, .var.name = "cdf is not valid")

  private$.parameters = ParameterSet$new(id = "data",
                                         value = list(data),
                                         support = UniversalSet$new(),
                                         settable = FALSE,
                                         updateFunc = NULL,
                                         description = "Data")

  super$initialize(decorators = decorators,
                   support = Set$new(data, class = "numeric"),
                   type = Reals$new())
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "WeightDisc", ClassName = "WeightedDiscrete",
                                                     Type = "\u211D", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "-"))
