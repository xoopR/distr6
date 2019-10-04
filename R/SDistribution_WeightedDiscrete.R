#' @include SetInterval_SpecialSet.R ParameterSet.R
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
#' @templateVar omittedVars skewness, kurtosis, entropy, mgf, cf
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
WeightedDiscrete <- R6::R6Class("WeightedDiscrete", inherit = SDistribution, lock_objects = F)
WeightedDiscrete$set("public","name","WeightedDiscrete")
WeightedDiscrete$set("public","short_name","WeightDisc")
WeightedDiscrete$set("public","description","WeightedDiscrete Probability Distribution.")
WeightedDiscrete$set("public","package","distr6")

WeightedDiscrete$set("public","mode",function(which = "all"){
  if(which == "all")
    return(private$.data$x[private$.data$pdf == max(private$.data$pdf)])
  else
    return(private$.data$x[private$.data$pdf == max(private$.data$pdf)][which])
})
WeightedDiscrete$set("public","mean",function(){
  return(sum(private$.data$x * private$.data$pdf))
})
WeightedDiscrete$set("public","variance",function(){
  return(sum((private$.data$x - self$mean())^2 * private$.data$pdf))
})

WeightedDiscrete$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  message("There are no parameters to set.")
  return(NULL)
})

WeightedDiscrete$set("private",".data",data.table::data.table())

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

  private$.data <- data

  pdf = cdf = quantile = rand = NULL

  pdf <- function(x1){
    return(as.numeric(unlist(private$.data[match(x1, private$.data$x), "pdf"])))
  }
  cdf <- function(x1){
    return(as.numeric(unlist(private$.data[findInterval(x1, private$.data$x), "cdf"])))
  }
  quantile <- function(p){
    mat = p <= matrix(private$.data$cdf, nrow = length(p), ncol = nrow(private$.data), byrow = T)
    which = apply(mat, 1, function(x) which(x)[1])
    which[is.na(which)] = ncol(mat)
    return(as.numeric(unlist(private$.data[which, "x"])))
  }
  rand <- function(n){
    return(sample(private$.data$x, n, TRUE, private$.data$pdf))
  }

  super$initialize(decorators = decorators, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
                   support = Set$new(private$.data$x),
                   symmetric = FALSE, type = Reals$new(),
                   valueSupport = "discrete",
                   variateForm = "univariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "WeightDisc", ClassName = "WeightedDiscrete",
                                                     Type = "\u211D", ValueSupport = "discrete",
                                                     VariateForm = "univariate",
                                                     Package = "distr6"))
