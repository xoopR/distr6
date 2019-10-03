#' @name VectorDistribution
#' @title Vectorise Distributions
#' @description A wrapper for creating a vector of distributions.
#'
#' @details A vector of distributions has the following relationship
#' \deqn{f_V(X1 = x1,...,XN = xN) = f_{X1}(x1), ..., f_{XN}(xn)}{f_V(X1 = x1,...,XN = xN) = f_X1(x1), ..., f_XN(xn)}
#' \deqn{F_V(X1 = x1,...,XN = xN) = F_{X1}(x1), ..., F_{XN}(xn)}{F_V(X1 = x1,...,XN = xN) = F_X1(x1), ..., F_XN(xn)}
#' where \eqn{f_V}/\eqn{F_V} is the pdf/cdf of the vector of distributions \eqn{V} and \eqn{X1,...,XN} are distributions.
#'
#' @section Constructor: VectorDistribution$new(distlist = NULL, distribution = NULL, params = NULL, name = NULL, short_name = NULL, description = NULL)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{distlist} \tab list \tab List of distributions. \cr
#' \code{distribution} \tab distribution \tab Distribution to wrap. \cr
#' \code{params} \tab a R object \tab Either list of parameters or matrix-type frame, see examples. \cr
#' \code{name} \tab list \tab Optional new name for distribution. \cr
#' \code{short_name} \tab list \tab Optional new short_name for distribution. \cr
#' \code{description} \tab list \tab Optional new description for distribution. \cr
#' }
#'
#' @section Constructor Details: A vector distribution can either be constructed by a list of
#' distributions passed to \code{distlist} or by passing the name of a distribution implemented in distr6
#' to \code{distribution}, as well as a list or table of parameters to \code{params}. The former case provides more flexibility
#' in the ability to use multiple distributions but the latter is useful for quickly combining many
#' distributions of the same type. See examples.
#'
#' @inheritSection DistributionWrapper Public Variables
#' @inheritSection DistributionWrapper Public Methods
#'
#' @seealso \code{\link{listWrappers}} and \code{\link{ProductDistribution}}
#'
#' @return Returns an R6 object of class VectorDistribution.
#'
#' @examples
#' vecBin <- VectorDistribution$new(list(Binomial$new(prob = 0.5,
#'                            size = 10), Normal$new(mean = 15)))
#' vecBin$pdf(x1 = 2, x2 =3)
#' vecBin$cdf(1:5, 12:16)
#' vecBin$quantile(c(0.1,0.2),c(0.3,0.4))
#' vecBin$rand(10)
#'
#' vecBin = VectorDistribution$new(distribution = Binomial,
#'        params = list(list(prob = 0.1, size = 2),
#'                    list(prob = 0.6, size = 4),
#'                    list(prob = 0.2, size = 6)))
#' vecBin$pdf(x1=1,x2=2,x3=3)
#' vecBin$cdf(x1=1,x2=2,x3=3)
#' vecBin$rand(10)
#'
#' #Equivalently
#' vecBin = VectorDistribution$new(distribution = Binomial,
#'        params = data.table::data.table(prob = c(0.1,0.6,0.2), size = c(2,4,6)))
#' vecBin$pdf(x1=1,x2=2,x3=3)
#' vecBin$cdf(x1=1,x2=2,x3=3)
#' vecBin$rand(10)
#'
#' @export
NULL
VectorDistribution <- R6::R6Class("VectorDistribution", inherit = DistributionWrapper, lock_objects = FALSE)
.distr6$wrappers <- append(.distr6$wrappers, list(VectorDistribution = VectorDistribution))

VectorDistribution$set("public","initialize",function(distlist = NULL, distribution = NULL, params = NULL,
                                                      name = NULL, short_name = NULL, description = NULL){

  if(is.null(distlist)){
    if(is.null(distribution) | is.null(params))
      stop("Either distlist or distribution and params must be provided.")

    distribution = paste0(substitute(distribution))
    if(!(distribution %in% listDistributions(simplify = T)))
      stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))

    distribution = get(distribution)
    if(checkmate::testList(params)){
      x <- params
      params <- data.table::as.data.table(t(data.table::as.data.table(x)))
      colnames(params) <- unique(names(unlist(x)))
    }

      if(inherits(params, "list") | inherits(params, "data.frame") | inherits(params, "matrix")){
      distlist = apply(params, 1, function(x) do.call(distribution$new, as.list(x)))
      name = paste0("Vector: ",nrow(params)," ",distribution$classname,"s")
      short_name = paste0("Vec",nrow(params),distribution$public_fields$short_name)
      description = paste("Vector of:",nrow(params),distribution$classname,"distributions")
    } else
      stop("params must inherit one of: list, data.frame, or matrix.")
  } else {
    if(is.null(name)) name = paste("Vector:",paste0(lapply(distlist, function(x) x$name),collapse=", "))
    if(is.null(short_name)) short_name = paste0(lapply(distlist, function(x) x$short_name),collapse="Vec")
    if(is.null(description)) description = paste0("Vector of:",paste0(lapply(distlist, function(x) x$description), collapse=" "))
  }

  distlist = makeUniqueDistributions(distlist)

  lst <- rep(list(bquote()), length(distlist))
  names(lst) <- paste("x",1:length(distlist),sep="")

  pdf = function() {}
  formals(pdf) = lst
  body(pdf) = substitute({
    pdfs = NULL
    for(i in 1:n)
      pdfs = c(pdfs,self$wrappedModels()[[i]]$pdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(pdfs, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    cdfs = NULL
    for(i in 1:n)
      cdfs = c(cdfs,self$wrappedModels()[[i]]$cdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(cdfs, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  quantile = function() {}
  formals(quantile) = lst
  body(quantile) = substitute({
    quantiles = NULL
    for(i in 1:n)
      quantiles = c(quantiles,self$wrappedModels()[[i]]$quantile(get(paste0("x",i))))
    y = data.table::data.table(matrix(quantiles, ncol = n))
    colnames(y) <- unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(y)
  },list(n = length(distlist)))

  rand = function(n) {
    rand <- sapply(self$wrappedModels(), function(x) x$rand(n))
    if(n == 1) rand <- t(rand)
    rand < data.table::as.data.table(rand)
    colnames(rand) = unlist(lapply(self$wrappedModels(), function(x) x$short_name))
    return(rand)
  }

  type = do.call(product.SetInterval, lapply(distlist,function(x) x$type()))
  support = do.call(product.SetInterval, lapply(distlist,function(x) x$support()))

  super$initialize(distlist = distlist, pdf = pdf, cdf = cdf, quantile = quantile, rand = rand, name = name,
                   short_name = short_name, description = description, support = support, type = type)
})
