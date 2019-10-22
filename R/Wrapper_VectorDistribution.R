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
#' vecBin = VectorDistribution$new(distribution = "Binomial",
#'        params = list(list(prob = 0.1, size = 2),
#'                    list(prob = 0.6, size = 4),
#'                    list(prob = 0.2, size = 6)))
#' vecBin$pdf(x1=1,x2=2,x3=3)
#' vecBin$cdf(x1=1,x2=2,x3=3)
#' vecBin$rand(10)
#'
#' #Equivalently
#' vecBin = VectorDistribution$new(distribution = "Binomial",
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

    # assumes distribution is a character
    if(!(any(distribution %in% listDistributions(simplify = T))))
      stop(paste(distribution, "is not currently implemented in distr6. See listDistributions()."))


    if(!checkmate::testList(params))
      params = apply(params, 1, as.list)

    private$.wrappedModels = data.table::data.table(distribution = distribution, params = params,
                                                    shortname = utils::getFromNamespace(distribution,"distr6")$public_fields$short_name)

    if(length(unique(distribution)) == 1)
      distribution = rep(distribution, length(params))

  } else {
    distlist = lapply(distlist, function(x) x$clone(deep = TRUE))
    private$.wrappedModels = data.table::data.table(distribution = distlist, params = NA,
                                                    shortname = sapply(distlist, function(x) x$short_name))
    distribution = sapply(distlist, function(x) x$name)
    private$.distlist = TRUE
  }

  ndist = nrow(private$.wrappedModels)

  if(length(unique(distribution)) == 1){
    if(is.null(name)) name = paste0("Vector: ", ndist," ",utils::getFromNamespace(distribution,"distr6")$classname,"s")
    if(is.null(short_name)) short_name = paste0("Vec", ndist,utils::getFromNamespace(distribution,"distr6")$public_fields$short_name)
    if(is.null(description)) description = paste("Vector of:", ndist,utils::getFromNamespace(distribution,"distr6")$classname,"distributions")
  } else{
    if(is.null(name)) name = paste("Vector:",paste0(distribution, collapse=", "))
    if(is.null(short_name)) short_name = paste0(distribution, collapse="Vec")
    if(is.null(description)) description = paste0("Vector of:",paste0(lapply(distlist, function(x) x$description), collapse=" "))
  }

  private$.wrappedModels[,3] <- makeUniqueNames(private$.wrappedModels[,3])

  lst <- rep(list(bquote()), ndist)
  names(lst) <- paste("x",1:ndist,sep="")

  pdf = function() {}
  formals(pdf) = lst
  body(pdf) = substitute({
    pdfs = NULL
    for(i in 1:n)
      pdfs = c(pdfs, self[i]$pdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(pdfs, ncol = n))
    colnames(y) <- unlist(self$wrappedModels()[,3])
    return(y)
  },list(n = ndist))

  cdf = function() {}
  formals(cdf) = lst
  body(cdf) = substitute({
    cdfs = NULL
    for(i in 1:n)
      cdfs = c(cdfs, self[i]$cdf(get(paste0("x",i))))
    y = data.table::data.table(matrix(cdfs, ncol = n))
    colnames(y) <- unlist(self$wrappedModels()[,3])
    return(y)
  },list(n = ndist))

  quantile = function() {}
  formals(quantile) = lst
  body(quantile) = substitute({
    quantiles = NULL
    for(i in 1:n)
      quantiles = c(quantiles, self[i]$quantile(get(paste0("x",i))))
    y = data.table::data.table(matrix(quantiles, ncol = n))
    colnames(y) <- unlist(self$wrappedModels()[,3])
    return(y)
  },list(n = ndist))

  rand = function(n) {
    rand <- sapply(1:nrow(self$wrappedModels()), function(x) self[x]$rand(n))
    if(n == 1) rand <- t(rand)
    rand <- data.table::as.data.table(rand)
    colnames(rand) <- unlist(self$wrappedModels()[,3])
    return(rand)
  }

  type = Reals$new(dim = ndist)
  support = Reals$new(dim = ndist)

  super$initialize(pdf = pdf, cdf = cdf, quantile = quantile, rand = rand, name = name,
                   short_name = short_name, description = description, support = support, type = type)
})

VectorDistribution$set("private", ".distlist", FALSE)

#' @title Extract one or more Distributions from a VectorDistribution
#' @description Once a \code{VectorDistribution} has been constructed, use \code{[}
#' to extract one or more \code{Distribution}s from inside it.
#' @param vecdist VectorDistribution from which to extract Distributions.
#' @param i indices specifying distributions to extract.
#' @export
Extract.VectorDistribution <- function(vecdist, i){
  i = i[i %in% (1:nrow(vecdist$wrappedModels()))]
  if(length(i) == 0)
    stop("index too large, should be less than or equal to ", nrow(vecdist$wrappedModels()))

  if(!vecdist$.__enclos_env__$private$.distlist){
    if(length(i) == 1){
      par = vecdist$wrappedModels()[i, 2][[1]]

      if(!checkmate::testList(par))
        par = list(par)

      return(do.call(get(vecdist$wrappedModels()[i, 1][[1]])$new, par))

    }else
      return(VectorDistribution$new(distribution = vecdist$wrappedModels()[i, 1],
                                    params = vecdist$wrappedModels()[i, 2]))
  } else {
    if(length(i) == 1)
      return(vecdist$wrappedModels()[i, 1][[1]])
    else
      return(VectorDistribution$new(distlist = vecdist$wrappedModels()[i, 1]))
  }
}

#' @rdname Extract.VectorDistribution
#' @usage \method{[}{VectorDistribution}(object, i)
#' @export
'[.VectorDistribution' <- function(vecdist, i){
  Extract.VectorDistribution(vecdist, i)
}

