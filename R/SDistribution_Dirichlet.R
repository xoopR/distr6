  #' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @name Dirichlet
#' @template SDist
#' @templateVar ClassName Dirichlet
#' @templateVar DistName Dirichlet
#' @templateVar uses as a prior in Bayesian modelling and is multivariate generalisation of the Beta distribution
#' @templateVar params concentration parameters, \eqn{\alpha_1,...,\alpha_k},
#' @templateVar pdfpmf pdf
#' @templateVar pdfpmfeq \deqn{f(x_1,...,x_k) = (\prod \Gamma(\alpha_i))/(\Gamma(\sum \alpha_i))\prod(x_i^{\alpha_i - 1})}
#' @templateVar paramsupport \eqn{\alpha = \alpha_1,...,\alpha_k; \alpha > 0}, where \eqn{\Gamma} is the gamma function
#' @templateVar distsupport \eqn{x_i \ \epsilon \ (0,1), \sum x_i = 1}{x_i \epsilon (0,1), \sum x_i = 1}
#' @templateVar omittedVars \code{mgf} and \code{cf}
#' @templateVar omittedDPQR \code{cdf} and \code{quantile}
#' @templateVar additionalDetails Sampling is performed via sampling independent Gamma distributions and normalising the samples (Devroye, 1986).
#' @templateVar constructor params = c(1, 1)
#' @templateVar arg1 \code{params} \tab numeric \tab vector of concentration parameters. \cr
#' @templateVar constructorDets \code{params} as a vector of positive numerics. The parameter \code{K} is automatically calculated by counting the length of the params vector, once constructed this cannot be changed.
#' @templateVar additionalReferences Devroye, Luc (1986). Non-Uniform Random Variate Generation. Springer-Verlag. ISBN 0-387-96305-7.
#' @templateVar additionalSeeAlso \code{\link{Beta}} for the Beta distribution.
#'
#' @examples
#' # Different parameterisations
#' x <- Dirichlet$new(params = c(2,5,6))
#'
#' # Update parameters
#' x$setParameterValue(params = c(3, 2, 3))
#' # 'K' parameter is automatically calculated
#' x$parameters()
#' \dontrun{
#' # This errors as less than three parameters supplied
#' x$setParameterValue(params = c(1, 2))
#' }
#'
#' # d/p/q/r
#' # Note the difference from R stats
#' x$pdf(0.1, 0.4, 0.5)
#' # This allows vectorisation:
#' x$pdf(c(0.3, 0.2), c(0.6, 0.9), c(0.9,0.1))
#' x$rand(4)
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
# Dirichlet Distribution Definition
#-------------------------------------------------------------
Dirichlet <- R6::R6Class("Dirichlet", inherit = SDistribution, lock_objects = F)
Dirichlet$set("public","name","Dirichlet")
Dirichlet$set("public","short_name","Diri")
Dirichlet$set("public","description","Multivariate Normal Probability Distribution.")
Dirichlet$set("public","package","distr6")

Dirichlet$set("public","mean",function(){
  return(self$getParameterValue("params")/sum(self$getParameterValue("params")))
})
Dirichlet$set("public","mode",function(){
  params <- self$getParameterValue("params")
  K <- self$getParameterValue("K")
  mode = rep(NaN, K)
  mode[params > 1] <- (params-1)/(sum(params)-K)
  return(mode)
})
Dirichlet$set("public","variance",function(){
  K <- self$getParameterValue("K")
  params <- self$getParameterValue("params")
  parami <- params/sum(params)
  var = (parami * (1 - parami))/(sum(params)+1)

  covar = matrix((-parami %*% t(parami))/(sum(params) + 1),nrow = K, ncol = K)
  diag(covar) = var
  return(covar)
})
Dirichlet$set("public","entropy",function(base = 2){
  params <- self$getParameterValue("params")
  return(log(prod(gamma(params))/gamma(sum(params)),2) + (sum(params) - length(params))*digamma(sum(params)) -
    sum((params-1)*digamma(params)))
})
Dirichlet$set("public", "pgf", function(z){
  return(NaN)
})

Dirichlet$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if("params" %in% names(lst)){
    checkmate::assert(length(lst$params) == self$getParameterValue("K"),
                      .var.name = "Number of categories cannot be changed after construction.")
  }
  super$setParameterValue(lst = lst, error = error)
  invisible(self)
})
Dirichlet$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$params)) lst = c(lst, list(params = paramlst$params))
  return(lst)
})

Dirichlet$set("public","initialize",function(params = c(1, 1), decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, params, verbose)
  self$setParameterValue(params = params)

  lst <- rep(list(bquote()), length(params))
  names(lst) <- paste("x",1:length(params),sep="")

  pdf <- function(){
    K <- self$getParameterValue("K")

    call <- mget(paste0("x",1:K))

    if(!all(unlist(lapply(call, is.numeric))))
      stop(paste(self$getParameterValue("K"),"arguments expected."))

    if(length(unique(unlist(lapply(call,length)))) > 1)
      stop("The same number of points must be passed to each variable.")

    args <- matrix(as.numeric(unlist(call)), ncol = K)

    params <- self$getParameterValue("params")
    p1 = prod(gamma(params))/gamma(sum(params))
    p1 = rep(p1, length(x1))

    p2 = args ^ matrix(params - 1, ncol = K, nrow = length(x1), byrow = T)
    p2 = apply(p2,1,prod)

    pdf = p2 / p1
    pdf[rowSums(args) != 1] = 0

    return(pdf)
  }
  formals(pdf) <- lst

  rand <- function(n){
    rand = sapply(params, function(x) rgamma(n, shape = x))
    if(n > 1)
      rand = apply(rand,1,function(x) x/sum(x))
    else
      rand = rand/sum(rand)

    return(data.table::data.table(t(rand)))
  }

  super$initialize(decorators = decorators, pdf = pdf, rand = rand,
                   support = Interval$new(0,1,type="()",dim = length(params)),
                   symmetric = FALSE,
                   type = Interval$new(0,1,dim = "K"),
                   valueSupport = "continuous",
                   variateForm = "multivariate")
  invisible(self)
})

.distr6$distributions = rbind(.distr6$distributions,
                              data.table::data.table(ShortName = "Diri", ClassName = "Dirichlet",
                                                     Type = "[0,1]^K", ValueSupport = "continuous",
                                                     VariateForm = "multivariate",
                                                     Package = "distr6"))

