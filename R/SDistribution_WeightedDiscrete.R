#' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name WeightedDiscrete
#' @template SDist
#' @templateVar ClassName WeightedDiscrete
#' @templateVar DistName WeightedDiscrete
#' @templateVar uses in empriical estimators such as Kaplan-Meier.
#' @templateVar pdfpmf pmf
#' @templateVar pdfpmfeq \deqn{f(x_i) = p_i}
#' @templateVar paramsupport \eqn{p_i, i = 1,\ldots,k; \sum p_i = 1}
#' @templateVar distsupport \eqn{x_1,...,x_k}
#' @templateVar additionalDetails Sampling from this distribution is performed with the \code{\link[base]{sample}} function with the elements given as the samples and the pdf as the probabilities. The cdf and quantile assumes that the elements are supplied in an indexed order (otherwise the results are meaningless).
#' @templateVar omittedVars skewness, kurtosis, entropy, mgf, cf
#' @templateVar constructor data.frame
#' @templateVar arg1 \code{data.frame} \tab data.frame \tab matrix-style object of observations and probabilities. See details. \cr
#' @templateVar constructorDets an object that can be coerced to a data.frame containing columns 'sample' and at least one of 'pdf' and 'cdf', see examples.
#' @templateVar additionalSeeAlso \code{\link[base]{sample}} for the sampling function and \code{\link{Empirical}} for the closely related Empirical distribution.
#'
#' @examples
#' x = WeightedDiscrete$new(data.frame = data.frame(samples = 1:3, pdf = c(1/5, 3/5, 1/5)))
#' WeightedDiscrete$new(data.frame = data.frame(samples = 1:3, cdf = c(1/5, 4/5, 1))) # equivalently
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
WeightedDiscrete <- R6::R6Class("WeightedDiscrete", inherit = SDistribution, lock_objects = F)
WeightedDiscrete$set("public","name","WeightedDiscrete")
WeightedDiscrete$set("public","short_name","GenDisc")
WeightedDiscrete$set("public","description","WeightedDiscrete Probability Distribution.")
WeightedDiscrete$set("public","package","distr6")

WeightedDiscrete$set("public","mode",function(which = "all"){
  if(which == "all")
    return(private$.data$samples[private$.data$pdf == max(private$.data$pdf)])
  else
    return(private$.data$samples[private$.data$pdf == max(private$.data$pdf)][which])
})
WeightedDiscrete$set("public","mean",function(){
  return(sum(private$.data$samples * private$.data$pdf))
})
WeightedDiscrete$set("public","variance",function(){
  return(sum((private$.data$samples - self$mean())^2)/nrow(private$.data))
})

WeightedDiscrete$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  message("There are no parameters to set.")
  return(NULL)
})

WeightedDiscrete$set("private",".data",data.table::data.table())

WeightedDiscrete$set("public","initialize",function(data.frame, decorators = NULL, verbose = FALSE){

  data.frame <- data.table::as.data.table(data.frame)
  checkmate::assert(all(colnames(data.frame) %in% c("pdf","cdf","samples")),
                    .var.name = "data.frame column names should be one of 'pdf', 'cdf', 'samples")
  checkmate::assert("samples" %in% colnames(data.frame),
                    .var.name = "'samples' must be included in data.frame column names")
  checkmate::assert(any(c("pdf","cdf") %in% colnames(data.frame)),
                    .var.name = "at least one of 'pdf' and 'cdf' must be included in data.frame column names")

  if("pdf" %in% colnames(data.frame) & !("cdf" %in% colnames(data.frame))){
    data.frame$cdf = cumsum(data.frame$pdf)
  } else if("cdf" %in% colnames(data.frame) & !("pdf" %in% colnames(data.frame))){
    data.frame$pdf = c(data.frame$cdf[1], diff(data.frame$cdf))
  }

  checkmate::assertNumeric(data.frame$pdf, lower = 0, upper = 1)
  checkmate::assertNumeric(data.frame$cdf, lower = 0, upper = 1)

  private$.data <- data.frame

  pdf = cdf = quantile = rand = NULL

  pdf <- function(x1){
    return(as.numeric(unlist(private$.data[match(x1, private$.data$samples), "pdf"])))
  }
  cdf <- function(x1){
    return(as.numeric(unlist(private$.data[findInterval(x1, private$.data$samples), "cdf"])))
  }
  quantile <- function(x1){
    return(as.numeric(unlist(private$.data[findInterval(x1, private$.data$cdf), "samples"])))
  }
  rand <- function(n){
    return(sample(private$.data$samples, n, TRUE, private$.data$pdf))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Set$new(private$.data$samples),
                   symmetric = FALSE, type = Reals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})
