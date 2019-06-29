  #' @include SetInterval_SpecialSet.R ParameterSet.R
#-------------------------------------------------------------
#  Distribution Documentation
#-------------------------------------------------------------
#' @title Dirichlet Distribution
#'
#' @description Mathematical and statistical functions for the Dirichlet distribution parameterised
#' with 'params' and defined by the pdf,
#' \deqn{f(x_1,...,x_k) = (\prod \Gamma(\alpha_i))/(\Gamma(\sum \alpha_i)) * \prod(x_i^{\alpha_i - 1})}
#' where \eqn{\alpha = \alpha_1,...,\alpha_k} are the k concentration parameters, \eqn{\Gamma} is the gamma function,
#' \eqn{\sum} is the summation function and \eqn{\prod} is the product function. The distribution is
#' supported on \eqn{x_i \epsilon (0,1), \sum x_i = 1}.
#'
#' @details mgf and cf are omitted as no closed form analytic expression could be found. The parameter
#' \code{K} is automatically updated by counting the length of the params vector, once constructed this
#' cannot be changed. Sampling is performed via sampling independent Gamma distributions and
#' normalising the samples.
#'
#' @name Dirichlet
#'
#' @section Constructor: Dirichlet$new(params, decorators = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{params} \tab numeric \tab vector of concentration parameters. \cr
#' \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#' \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#' }
#'
#' @section Constructor Details: There is no default parameterisation.
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @examples
#' # Different parameterisations
#' x <- Dirichlet$new(params = c(2,5,6))
#'
#' # Update parameters
#' x$setParameterValue(list(params = c(3, 2, 3)))
#' x$parameters() # Note the K parameter is automatically calculated
#' \dontrun{
#' # This errors as less than three parameters supplied
#' x$setParameterValue(list(params = c(1, 2)))
#' }
#'
#' # p/d/q/r
#' # Note the difference from R stats
#' x$pdf(0.1, 0.4, 0.5)
#' # This allows vectorisation:
#' x$pdf(c(0.3, 0.2), c(0.6, 0.9), c(0.9,0.1))
#' x$rand(4)
#'
#' # Statistics
#' x$mean()
#' x$var()
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
Dirichlet$set("public","traits",list(type = Interval$new(0,1,dim = "K"),
                                  valueSupport = "continuous",
                                  variateForm = "multivariate"))
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
Dirichlet$set("public","var",function(){
  K <- self$getParameterValue("K")
  params <- self$getParameterValue("params")
  parami <- params/sum(params)
  var = (parami * (1 - parami))/(sum(params)+1)

  covar = matrix((-parami %*% t(parami))/(sum(params) + 1),nrow = K, ncol = K)
  diag(covar) = var
  return(covar)
})
Dirichlet$set("public","cor",function(){
  return(self$var() / (sqrt(diag(self$var()) %*% t(diag(self$var())))))
})
Dirichlet$set("public","entropy",function(base = 2){
  params <- self$getParameterValue("params")
  return(log(prod(gamma(params))/gamma(sum(params)),2) + (sum(params) - length(params))*digamma(sum(params)) -
    sum((params-1)*digamma(params)))
})

Dirichlet$set("public","setParameterValue",function(lst, error = "warn"){
  if("params" %in% names(lst)){
    checkmate::assert(length(lst$params) == self$getParameterValue("K"),
                      .var.name = "Number of categories cannot be changed after construction.")
  }
  super$setParameterValue(lst, error)
})
Dirichlet$set("private",".getRefParams", function(paramlst){
  lst = list()
  if(!is.null(paramlst$params)) lst = c(lst, list(params = paramlst$params))
  return(lst)
})

Dirichlet$set("public","initialize",function(params, decorators = NULL, verbose = FALSE){

  private$.parameters <- getParameterSet(self, params, verbose)
  self$setParameterValue(list(params = params))

  lst <- rep(list(bquote()), length(params))
  names(lst) <- paste("x",1:length(params),sep="")

  pdf <- function(){
    K <- self$getParameterValue("K")

    call <- mget(paste0("x",1:K))
    if(length(unique(unlist(lapply(call,length)))) > 1){
      stop("The same number of points must be passed to each variable.")
    }
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
                   distrDomain = Reals$new(dim = length(params)), symmetric = FALSE)
  invisible(self)
})
