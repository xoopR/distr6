##-------------------------------------------------------------
# Distribution Documentation
#-------------------------------------------------------------
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#' @name Distribution
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{name} \tab character \tab Full name of distribution. \cr
#' \code{short_name} \tab character \tab Short name to identify distribution. \cr
#' \code{type} \tab SetInterval \tab Scientific type. \cr
#' \code{support} \tab SetInterval \tab Distribution support. See Details. \cr
#' \code{distrDomain} \tab SetInterval \tab Distribution domain See Details. \cr
#' \code{symmetric} \tab logical \tab Is distribution symmetric? \cr
#' \code{pdf} \tab function \tab See Details. \cr
#' \code{cdf} \tab function \tab See Details. \cr
#' \code{quantile} \tab function \tab See Details. \cr
#' \code{rand} \tab function \tab See Details. \cr
#' \code{parameters} \tab ParameterSet \tab See Details. \cr
#' \code{decorators} \tab list \tab R6 decorators to add in construction. \cr
#' \code{valueSupport} \tab character \tab continuous, discrete, mixture. See Details. \cr
#' \code{variateForm} \tab character \tab univariate, multivariate, matrixvariate. See Details. \cr
#' \code{description} \tab character \tab short description of distribution. \cr
#' \code{R62S3} \tab logical \tab if TRUE (default), S3 methods are added for decorators in construction.
#' }
#'
#' @section Constructor Details: The most basic Distribution object consists of a name and one of pdf/cdf.
#'
#'   If supplied, \code{type}, \code{support} and \code{distrDomain} should be given as an R6 SetInterval
#'   object. If none are supplied then the set of Reals is taken to be the type, support and domain
#'   of the distribution. If only \code{type} is supplied then this is taken to also be the support
#'   and domain.
#'
#'   By default, missing \code{pdf}, \code{cdf}, \code{quantile} and \code{rand} are not automatically imputed.
#'   Use the \code{\link{FunctionImputation}} decorator to generate these with a selected method.
#'
#'   If the distribution has parameters, then these should be supplied as a ParameterSet.
#'   The distribution parameterisation is taken to be whichever parameters are flagged as 'settable',
#'   any others in the ParameterSet are automatically updated by a given function. See \code{\link{ParameterSet}}
#'   for more details on construction of a ParameterSet.
#'
#'   \code{decorators} is a list of decorators (R6 environments not strings) to decorate the
#'   Distribution with in construction. Decorators can also be added after construction. See
#'   \code{\link{DistributionDecorator}} for more details. The \code{R62S3} determines if S3 methods
#'   should be added for the given decorator, it is ignored if \code{decorators = NULL}.
#'
#'   \code{valueSupport} should be one of continuous/discrete/mixture if supplied.
#'   \code{variateForm} should be one of univariate/multivariate/matrixvariate if supplied.
#'   If not given these are automatically filled from \code{type} and \code{support}.
#'
#' @section Accessor Methods:
#'  \tabular{lll}{
#'   \strong{Method} \tab \strong{Return Type} \tab \strong{Details} \cr
#'   \code{name()} \tab character \cr
#'   \code{short_name()} \tab character \cr
#'   \code{description()} \tab character \cr
#'   \code{decorators()} \tab character \cr
#'   \code{traits()} \tab list \cr
#'   \code{valueSupport()} \tab character \cr
#'   \code{variateForm()} \tab character \cr
#'   \code{type()} \tab Set \tab \code{\link{Set}} \cr
#'   \code{properties()} \tab list \cr
#'   \code{support()} \tab Set \tab \code{\link{Set}} \cr
#'   \code{distrDomain()} \tab Set \tab \code{\link{Set}} \cr
#'   \code{symmetry()} \tab character \cr
#'   \code{parameters(id)} \tab ParameterSet or data.frame. \tab \code{\link{ParameterSet}} \cr
#'   \code{getParameterValue(id)} \tab numeric \tab \code{\link{ParameterSet}} \cr
#'   \code{sup()} \tab numeric \tab supremum of distribution \cr
#'   \code{inf()} \tab numeric \tab infimum of distribution \cr
#'   }
#'
#' @section Math/Stats Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Details} \cr
#'   \code{pdf(x1, log = F)} \tab Evaluate density/mass at x1 \cr
#'   \code{cdf(x1, lower.tail = T, log.p = F)} \tab Evaluate distribution function at x1.\cr
#'   \code{quantile(p, lower.tail = T, log.p = F)} \tab Evaluate quantile function at p \cr
#'   \code{rand(n)} \tab Simulate n draws from distribution \cr
#'   \code{expectation(trafo)} \tab Calculate expectation \cr
#'   \code{var()} \tab Calculate variance \cr
#'   \code{sd()} \tab Calculate standard deviation \cr
#'   \code{cov()} \tab Calculate covariance. See Details \cr
#'   \code{cor()} \tab Calculate correlation. See Details \cr
#'   \code{median()} \tab Calculate median \cr
#'  }
#'
#' @section Other Methods:
#'  \tabular{lll}{
#'   \strong{Method} \tab \strong{Input -> Output} \tab \strong{Details} \cr
#'   \code{setParameterValue(lst)} \tab list -> invisible(self) \tab Set parameter value. See \code{\link{ParameterSet}}. \cr
#'   \code{liesInSupport(x, all = TRUE)} \tab numeric x logical -> logical \tab Does x lie in the support of distribution? See Details. \cr
#'   \code{liesInType(x)} \tab numeric -> logical \tab Does x lie in the type of distribution? \cr
#'   \code{liesInDistrDomain(x)} \tab numeric -> logical \tab Does x lie in the domain of distribution? \cr
#' }
#'
#' @section Representation Methods:
#' \tabular{ll}{
#'   \strong{Method} \tab \strong{Details} \cr
#'   \code{strprint()} \tab Character representation of print \cr
#'   \code{print()} \tab Print method \cr
#'   \code{summary(full = T)} \tab Summary method \cr
#'   \code{plot()} \tab Plotting method \cr
#'   \code{qqplot()} \tab QQ-Plots \cr
#' }
#'
#'
#' @section Public Methods Details:
#' \code{cov} defaults to \code{var} for univariate distributions and \code{cor} returns NULL.
#'
#' If \code{liesInSupport(x, all = TRUE)} then returns TRUE only if every numeric in vector \code{x} lies
#' in the support of the distribution, otherwise returns a vector of logicals.
#'
#'
#' @seealso See \code{\link{SetInterval}} and \code{\link{SpecialSet}} for details on Sets and
#' Intervals. See \code{\link{ParameterSet}} for parameter details. See
#' \code{\link{DistributionDecorator}} for Decorator details. See \code{\link[stats]{Binomial}} for
#' details on the arguments to \code{pdf}/\code{cdf}/\code{quantile}/\code{rand}.
NULL
#-------------------------------------------------------------
#-------------------------------------------------------------
# Distribution Definition
#-------------------------------------------------------------
#' @include Distribution_helpers.R SetInterval_helpers.R
#' @export
Distribution <- R6::R6Class("Distribution", lock_objects = FALSE)

#-------------------------------------------------------------
# Distribution Private Methods
#-------------------------------------------------------------
Distribution$set("private",".setWorkingSupport",function(){
  suppressMessages({
    rands = self$rand(20)

    if(self$sup() != Inf)
      newsup = self$sup()
    else{
      newsup = max(rands)
      while(self$pdf(newsup) > .Machine$double.eps) newsup = newsup + 1
      newsup = ceiling(newsup - 1)
    }

    inf = self$inf()
    if(inf != -Inf)
      newinf = inf
    else{
      newinf = min(rands)
      while(self$pdf(newinf) > .Machine$double.eps) newinf = newinf - 1
      newinf = floor(newinf + 1)
    }

    private$.workingSupport <- list(inf = newinf, sup = newsup)
  })
}) # NEEDS TESTING
Distribution$set("private",".getWorkingSupportRange",function(){
  return(private$.workingSupport$inf:private$.workingSupport$sup)
}) # NEEDS TESTING

#-------------------------------------------------------------
# Distribution Public Methods
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name = NULL, short_name = NULL,
                      type = NULL, support = NULL, distrDomain = NULL,
                      symmetric = logical(0),
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters = NULL, decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL, additionalMethods = NULL, R62S3 = TRUE
                      ){

  if(RSmisc::getR6Class(self) == "Distribution" | inherits(self,"DistributionWrapper")){

    if(is.null(pdf) & is.null(cdf))
      stop("One of pdf or cdf must be provided.")

    # Validation checks
    if(is.null(name) & is.null(short_name))
      checkmate::assert("One of 'name' or 'short_name' must be provided.")
    if(is.null(short_name)) short_name = gsub(" ","",name,fixed = T)
    if(is.null(name)) name = short_name
    checkmate::assertCharacter(c(name, short_name),
                               .var.name = "'name' and 'short_name' must be of class 'character'.")
    checkmate::assert(length(strsplit(short_name,split=" ")[[1]])==1,
                      .var.name = "'short_name' must be one word only.")

    checkmate::assertLogical(symmetric)

    self$name <- name

    self$short_name <- short_name

    if(!is.null(description))
      self$description <- description

    if(is.null(type)){
      if(!is.null(pdf)) type <- Reals$new(dim = length(formals(pdf)))
      else type <- Reals$new(dim = length(formals(cdf)))
    }
    if(is.null(support)) support <- type
    if(is.null(distrDomain)) distrDomain <- type
    checkmate::assert(inherits(type,"SetInterval"), inherits(support,"SetInterval"),
                      inherits(distrDomain,"SetInterval"),
                      .var.name = "'type', 'support' and 'distrDomain' should be class 'SetInterval'.")

    if(!is.null(valueSupport)){
      if(grepl("^c",valueSupport))
        valueSupport = "continuous"
      else if(grepl("^d",valueSupport))
        valueSupport = "discrete"
      else if(grepl("^m",valueSupport))
        valueSupport = "mixture"
      else
        stop("valueSupport should be one of: 'continuous', 'discrete','mixture'.")
    } else if(class(support)[[1]] %in% c("Reals","PosReals","NegReals","Rationals","PosRationals",
                                         "NegRationals","Interval"))
      valueSupport = "continuous"
    else
      valueSupport = "discrete"

    if(!is.null(variateForm)){
      if(grepl("^u",variateForm)) variateForm = "univariate"
      else if(grepl("^mu",variateForm)) variateForm = "multivariate"
      else if(grepl("^ma",variateForm)) variateForm = "matrixvariate"
      else stop("variateForm should be one of: 'univariate', 'multivariate','matrixvariate'.")
    } else if(type$dimension() == 1)
      variateForm = "univariate"
    else
      variateForm = "multivariate"

    self$traits <- c(self$traits, type = type)
    self$traits <- c(self$traits, valueSupport = valueSupport)
    self$traits <- c(self$traits, variateForm = variateForm)

    self$properties <- c(self$properties, support = support)
    self$properties <- c(self$properties, distrDomain = distrDomain)
    symm = ifelse(symmetric,"symmetric","asymmetric")
    self$properties <- c(self$properties, symmetry = symm)

    if(!is.null(pdf)){
      if(!is.null(formals(pdf)$self))
        formals(pdf)$self = self
      else
        formals(pdf) = c(formals(pdf),list(self=self),alist(...=))
      private$.setPdf(pdf)
    }

    if(!is.null(cdf)){
      if(!is.null(formals(cdf)$self))
        formals(cdf)$self = self
      else
        formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
      private$.setCdf(cdf)
    }

    if(!is.null(pdf) & !is.null(cdf)){
      checkmate::assert(length(formals(pdf)) == length(formals(cdf)),
                        .var.name = "'pdf' and 'cdf' maust take the same arguments.")
      checkmate::assert(all(names(formals(pdf)) == names(formals(cdf))),
                        .var.name = "'pdf' and 'cdf' maust take the same arguments.")
    }


    if(!is.null(quantile)){
      if(!is.null(formals(quantile)$self))
        formals(quantile)$self = self
      else
        formals(quantile) = c(formals(quantile),list(self=self),alist(...=))
      private$.setQuantile(quantile)
    }

    if(!is.null(rand)){
      if(!is.null(formals(rand)$self))
        formals(rand)$self = self
      else
        formals(rand) = c(formals(rand),list(self=self),alist(...=))
      private$.setRand(rand)
    }

    if(!is.null(parameters)){
      checkmate::assertClass(parameters,"ParameterSet")
      if(!inherits(self, "DistributionWrapper"))
        private$.parameters <- parameters$clone()$update()
      else
        private$.parameters <- parameters$clone()
    }
  }

    if(!is.null(decorators))
      suppressMessages(decorate(self, decorators, R62S3))

    # Update skewness and kurtosis
  unlockBinding("properties",self)
    x = try(self$kurtosis(excess = TRUE), silent = TRUE)
    if(class(x) == "try-error")
      self$properties$kurtosis <- NULL
    else
      self$properties$kurtosis <- exkurtosisType(x)

    x = try(self$skewness(), silent = TRUE)
    if(class(x) == "try-error")
      self$properties$skewness <- NULL
    else
      self$properties$skewness <- skewType(x)

    # private$.setWorkingSupport()
    lockBinding("name",self)
    lockBinding("short_name",self)
    lockBinding("description",self)
    lockBinding("traits",self)
    lockBinding("properties",self)
    lockBinding("parameters",self)
    lockBinding("decorators",self)

  invisible(self)
}) # IN PROGRESS/NEEDS TESTING

Distribution$set("public","strprint",function(){
  if(length(private$.parameters)!=0){
    string = paste(apply(self$parameters()$as.data.frame()[self$parameters()$as.data.frame()$settable,],1,
                         function(x) paste(x[1],trimws(x[2]),sep=" = ")
                         ),
                   collapse=", ")
    string = paste0(self$short_name,"(",string,")")
  } else {
    string = paste0(self$short_name)
  }
  return(string)
}) # DONE
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
}) # DONE
Distribution$set("public","summary",function(full=T){

  which_params = self$parameters()$as.data.frame()$settable

  if(full){
    if(length(private$.parameters)!=0){
      cat(self$name,"with parameterisation:\n")
      cat("\t",paste(self$parameters()$as.data.frame()[which_params, "id"][[1]],
                     self$parameters()$as.data.frame()[which_params,"value"][[1]],
                     sep = " = ", collapse = "; "))
    } else
      cat(self$name(),"\n")
    cat("\n\n Quick Statistics: \n")

    a_exp = suppressMessages(try(self$expectation(), silent = T))
    a_var = suppressMessages(try(self$var(), silent = T))
    a_skew = suppressMessages(try(self$skewness(), silent = T))
    a_kurt = suppressMessages(try(self$kurtosis(), silent = T))

    if(!inherits(a_exp,"try-error")) cat("\tMean")
    if(!inherits(a_var,"try-error")) cat("\tVariance")
    if(!inherits(a_skew,"try-error")) cat("\tSkewness")
    if(!inherits(a_kurt,"try-error")) cat("\tExcess Kurtosis")
    cat("\n")

    if(!inherits(a_exp,"try-error")) cat("\t", a_exp, sep = "")
    if(!inherits(a_var,"try-error")) cat("\t", a_var, sep = "")
    if(!inherits(a_skew,"try-error")) cat("\t\t", a_skew, sep = "")
    if(!inherits(a_kurt,"try-error")) cat("\t\t", a_kurt, sep = "")
    cat("\n")

    cat(" Support:",self$support()$getSymbol())
    cat("\n Traits: ",self$valueSupport(),"; ",self$variateForm(),"\n\t See getTraits() for more",sep="")

    if(inherits(a_kurt,"try-error"))
      cat("\n Properties: ", self$distrDomain()$getSymbol(), "; ", self$symmetry(),
          "\n\t See getProperties() for more", sep="")
    else
      cat("\n Properties: ", self$kurtosisType(), "; ", self$skewnessType(),"; ", self$symmetry(),
          "\n\t See getProperties() for more", sep="")

    if(!is.null(self$decorators))
      cat("\n\n Decorated with: ", paste0(self$decorators,collapse=", "))

  } else {
    if(length(private$.parameters)!=0){
      cat(self$short_name,"distribution with parameterisation: ")
      cat(paste(self$parameters()$as.data.frame()[which_params,"id"][[1]],
                self$parameters()$as.data.frame()[which_params,"value"][[1]],
                sep = " = ", collapse = "; "))
    } else
        cat(self$name)
    cat("\n Scientific Type:",self$type()$getSymbol(),"\t See getTraits() for more")
    cat("\n Support:",self$support()$getSymbol(),"\t\t See getProperties() for more")
  }
}) # NEEDS TESTING
Distribution$set("public","plot",function(){}) # TO DO
Distribution$set("public","qqplot",function(){}) # TO DO

# Details Accessors
Distribution$set("public","name",character(0))
Distribution$set("public","short_name",character(0))
Distribution$set("public","description",NULL)
Distribution$set("public","decorators",list())

# Traits Accessors
Distribution$set("public","traits",list())
Distribution$set("public","valueSupport",function(){
  return(self$traits[["valueSupport"]])
})
Distribution$set("public","variateForm",function(){
  return(self$traits[["variateForm"]])
})
Distribution$set("public","type",function(){
  return(self$traits[["type"]])
})

# `Properties` Accessors
Distribution$set("public","properties",list())
Distribution$set("public","support",function(){
  return(self$properties[["support"]])
})
Distribution$set("public","distrDomain",function(){
  return(self$properties[["distrDomain"]])
})
Distribution$set("public","symmetry",function(){
  return(self$properties[["symmetry"]])
})

# Parameter Accessors
Distribution$set("public","parameters",function(id = NULL){
  return(private$.parameters$parameters(id))
}) # DONE
Distribution$set("public","getParameterValue",function(id){
  return(private$.parameters$getParameterValue(id))
}) # DONE
Distribution$set("public","setParameterValue",function(lst){

  self$parameters()$setParameterValue(lst)

  # Update skewness and kurtosis
  unlockBinding("properties", self)
  x = try(self$kurtosis(excess = TRUE), silent = TRUE)
  if(class(x) == "try-error")
    self$properties$kurtosis <- NULL
  else
    self$properties$kurtosis <- exkurtosisType(x)

  x = try(self$skewness(), silent = TRUE)
  if(class(x) == "try-error")
    self$properties$skewness <- NULL
  else
    self$properties$skewness <- skewType(x)
  lockBinding("properties", self)

  #private$.setWorkingSupport()
  invisible(self)
}) # DONE

# p/d/q/r
Distribution$set("public","pdf",function(x1, ..., log = FALSE){
  if(testUnivariate(self)){
    pdf = x1
    pdf[!self$liesInSupport(x1, all = F)] = 0

    if(all(pdf==0)) return(0)

    pdf.in = sapply(pdf[self$liesInSupport(x1, all = F)], function(x0) private$.pdf(x0,...))

    pdf[self$liesInSupport(x1, all = F)] = pdf.in
  } else {
    if(is.null(x1)) pdf = private$.pdf(...)
    else pdf = private$.pdf(x1, ...)
  }

  pdf = unlist(pdf)

  if(log) return(log(pdf))
  else return(pdf)
}) # NEEDS TESTING
Distribution$set("public","cdf",function(x1, lower.tail = TRUE, log.p = FALSE,...){

  if(testUnivariate(self)){
    cdf = x1
    cdf[x1 >= self$sup()] = 1
    cdf[x1 < self$inf()] = 0

    cdf.in = sapply(cdf[x1 < self$sup() & x1 >= self$inf()], function(q0) private$.cdf(q0,...))

    cdf[x1 < self$sup() & x1 >= self$inf()] = cdf.in
  } else {
    if(is.null(x1)) cdf = private$.cdf(...)
    else cdf = private$.cdf(x1, ...)
  }

  cdf = unlist(cdf)

  if(log.p & lower.tail) return(log(cdf))
  else if(log.p & !lower.tail) return(log(1 - cdf))
  else if(!log.p & lower.tail) return(cdf)
  else return(1 - cdf)
}) # NEEDS TESTING
Distribution$set("public","quantile",function(p, ..., lower.tail = TRUE){
    if(lower.tail){
      return(unlist(sapply(p, function(p0) private$.quantile(p0,...))))
    } else{
      return(unlist(sapply(p, function(p0) private$.quantile(1 - p0,...))))
    }
}) # NEEDS TESTING
Distribution$set("public","rand",function(n){
  return(private$.rand(n))
}) # NEEDS TESTING

# Analytic Maths/stats
Distribution$set("public","sd",function(){
  return(sqrt(self$var()))
}) # DONE
Distribution$set("public","median",function(){
  self$quantile(0.5)
}) # DONE
Distribution$set("public","sup",function(){
  return(self$support()$sup())
}) # DONE
Distribution$set("public","inf",function(){
  return(self$support()$inf())
}) # DONE

Distribution$set("public", "kurtosisType", function() {
  x = self$properties$kurtosis
  if(inherits(x,"try-error"))
    return(NA)
  else
    return(x)
})
Distribution$set("public", "skewnessType", function() {
  x = self$properties$skewness
  if(inherits(x,"try-error"))
    return(NA)
  else
    return(x)
})

# Validation Checks
Distribution$set("public","liesInSupport",function(x, all = TRUE){
  if(all)
    return(all(x >= self$inf()) & all(x <= self$sup()))
  else
    return(x >= self$inf() & x <= self$sup())
}) # DONE
Distribution$set("public","liesInType",function(x){
  return(all(x >= self$type()$lower()) & all(x <= self$type()$upper))
}) # NEEDS TESTING
Distribution$set("public","liesInDistrDomain",function(x){
  return(all(x >= self$distrDomain()$lower()) & all(x <= self$distrDomain()$upper))
}) # NEEDS TESTING

#-------------------------------------------------------------
# Distribution Private Variables
#-------------------------------------------------------------
Distribution$set("private",".pdf", function(...) return(NULL))
Distribution$set("private",".cdf", function(...) return(NULL))
Distribution$set("private",".quantile", function(...) return(NULL))
Distribution$set("private",".rand", function(...) return(NULL))
Distribution$set("private",".setPdf",function(pdf){
  unlockBinding(".pdf",private)
  private$.pdf <- pdf
  lockBinding(".pdf",private)
})
Distribution$set("private",".setCdf",function(cdf){
  unlockBinding(".cdf",private)
  private$.cdf <- cdf
  lockBinding(".cdf",private)
})
Distribution$set("private",".setQuantile",function(quantile){
  unlockBinding(".quantile",private)
  private$.quantile <- quantile
  lockBinding(".quantile",private)
})
Distribution$set("private",".setRand",function(rand){
  unlockBinding(".rand",private)
  private$.rand <- rand
  lockBinding(".rand",private)
})
Distribution$set("private",".parameters",data.frame())
Distribution$set("private",".workingSupport",NULL) # DONE

