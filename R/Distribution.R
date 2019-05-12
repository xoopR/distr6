#-------------------------------------------------------------
# Distribution R6Class Definition
#-------------------------------------------------------------
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#
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
#' \code{description} \tab character \tab short description of distribution.
#' }
#'
#' @section Constructor Details: The primary purpose of the Distribution object is to serve as the parent class
#'   to all other distributions, therefore all methods are approximate numeric calculations
#'   and the user may prefer to utilise decorators to improve accuracy.
#'
#'   \code{type}, \code{support} and \code{distrDomain} should be given as an R6 SetInterval
#'   object. If none are supplied then the set of Reals is taken to be the type, support and domain
#'   of the distribution. If only \code{type} is supplied then this is taken to also be the support
#'   and domain.
#'
#'   By default, missing \code{pdf}, \code{cdf} and \code{quantile} are not automatically imputed.
#'   Use the imputation wrappers (see below) to generate these with a selected method.
#'   The \code{rand} function is automatically generated depending on which of the above are supplied.
#'   The generation for this is performed according to the hierarchy: quantile -> rand, cdf -> rand, pdf -> rand.
#'
#'   \code{parameters} should be supplied as a ParameterSet. The distribution parameterisation
#'   is taken to be whichever parameters are flagged as 'settable', any others in the ParameterSet
#'   are automatically updated by a given function.
#'
#'   \code{decorators} is a list of decorators (R6 environments not strings) to decorate the
#'   Distribution with in construction. Decorators can also be added after construction. See
#'   \code{\link{DistributionDecorator}} for more details.
#'
#'   \code{valueSupport} should be one of continuous/discrete/mixture if supplied.
#'   \code{variateForm} should be one of univariate/multivariate/matrixvariate if supplied.
#'   If not given these are automatically filled from \code{type} and \code{support}.
#'
#' @section Accessor Methods:
#'  \tabular{lrr}{
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
#'   \code{parameters(id,as.df = FALSE)} \tab ParameterSet or data.frame \tab \code{\link{ParameterSet}} \cr
#'   \code{getParameterValue(id)} \tab numeric \tab \code{\link{ParameterSet}} \cr
#'   \code{sup()} \tab numeric \tab supremum of distribution \cr
#'   \code{inf()} \tab numeric \tab infimum of distribution \cr
#'   }
#'
#' @section Math/Stats Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Details} \cr
#'   \code{pdf(x, log = F)} \tab Evaluate density/mass at x \cr
#'   \code{cdf(q, lower.tail = T, log.p = F)} \tab Evaluate distribution function at q.\cr
#'   \code{quantile(p, lower.tail = T, log.p = F)} \tab Evaluate quantile function at p \cr
#'   \code{rand(n)} \tab Simulate n draws from distribution \cr
#'   \code{expectation(trafo)} \tab Calculate expectation \cr
#'   \code{var()} \tab Calculate variance \cr
#'   \code{sd()} \tab Calculate standard deviation \cr
#'   \code{cov()} \tab Calculate covariance. See Details \cr
#'   \code{cor()} \tab Calculate correlation. See Details \cr
#'   \code{median()} \tab Calculate median \cr
#'   \code{mode(which = 1)} \tab Calculate mode. See Details \cr
#'  }
#'
#' @section Other Methods:
#'  \tabular{lrr}{
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
#' \code{mode} returns by default the first mode of the distribution where applicable, otherwise a specified
#' integer or all.
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

#' @include R6_helpers.R Distribution_helpers.R SetInterval_helpers.R
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
Distribution$set("public","initialize",function(name, short_name,
                      type = Reals$new(), support, distrDomain,
                      symmetric = logical(0),
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters, decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL, additionalMethods = NULL
                      ){

  if(getR6Class(self) == "Distribution" | inherits(self,"DistributionWrapper")){

    # Validation checks
    if(missing(short_name)) short_name = gsub(" ","",name,fixed = T)
    checkmate::assertCharacter(c(name, short_name),
                               .var.name = "'name' and 'short_name' must be of class 'character'.")
    checkmate::assert(length(strsplit(short_name,split=" ")[[1]])==1,
                      .var.name = "'short_name' must be one word only.")

    checkmate::assertLogical(symmetric)

    self$name <- name

    self$short_name <- short_name

    if(!is.null(description))
      self$description <- description

    if(missing(support))
      support = type
    if(missing(distrDomain))
      distrDomain = type

    checkmate::assert(inherits(type,"Set"), .var.name = "type should be class 'Set'.")
    checkmate::assert(inherits(support,"Set"), inherits(distrDomain,"Set"),
                      .var.name = "'support' and 'distrDomain' should be class 'Set'.")

    if(!is.null(valueSupport))
      checkmate::assert(valueSupport == "continuous", valueSupport == "discrete",
                        valueSupport == "mixture",
                        .var.name = "valueSupport should be one of: 'continuous', 'discrete',
                        'mixture'.")
    else if(class(support)[[1]] %in% c("Reals","PosReals","NegReals"))
      valueSupport = "continuous"
    else
      valueSupport = "discrete"

    if(!is.null(variateForm))
      checkmate::assert(variateForm == "univariate", variateForm == "multivariate",
                        variateForm == "matrixvariate",
                        .var.name = "variateForm should be one of: 'univariate', 'multivariate',
                        'matrixvariate'.")
    else if(type$dimension() == 1)
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
      private$.pdf <- pdf
    } else
      private$.pdf <- function(...){
        return(NULL)
      }

    if(!is.null(cdf)){
      if(!is.null(formals(cdf)$self))
        formals(cdf)$self = self
      else
        formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
      private$.cdf <- cdf
    } else
      private$.cdf <- function(...){
        return(NULL)
      }

    if(!is.null(quantile)){
      if(!is.null(formals(quantile)$self))
        formals(quantile)$self = self
      else
        formals(quantile) = c(formals(quantile),list(self=self),alist(...=))
      private$.quantile <- quantile
    } else
      private$.quantile <- function(...){
        return(NULL)
      }

    if(!is.null(rand)){
      if(!is.null(formals(rand)$self))
        formals(rand)$self = self
      else
        formals(rand) = c(formals(rand),list(self=self),alist(...=))
      private$.rand <- rand
    } else
      private$.rand <- function(...){
        return(NULL)
      }

    if(!missing(parameters)){
      checkmate::assertClass(parameters,"ParameterSet")
      if(!inherits(self, "DistributionWrapper"))
        private$.parameters <- parameters$clone()$update()
      else
        private$.parameters <- parameters$clone()
    }
  }

    if(!is.null(decorators))
      suppressMessages(decorate(self, decorators))

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
    string = paste(apply(self$parameters(as.df = T)[self$parameters(as.df = T)$settable,],1,
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

  which_params = self$parameters(as.df = T)$settable

  if(full){
    if(length(private$.parameters)!=0){
      cat(self$name,"with parameterisation:\n")
      cat("\t",paste(self$parameters(as.df = T)[which_params, "id"][[1]],
                     self$parameters(as.df = T)[which_params,"value"][[1]],
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
      cat(paste(self$parameters(as.df = T)[which_params,"id"][[1]],
                self$parameters(as.df = T)[which_params,"value"][[1]],
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

# Properties Accessors
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
Distribution$set("public","parameters",function(id,as.df = F){
  return(private$.parameters$parameters(id, as.df))
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
Distribution$set("public","pdf",function(x, log = FALSE){

  if(is.null(private$.pdf(1)))
    return(NULL)

  y = x

  y[!self$liesInSupport(x, F)] = 0

  if(all(y == 0))
    return(y)
  else{

    if(log)
      y[self$liesInSupport(x, F)] = log(private$.pdf(x[self$liesInSupport(x, F)], self = self))
    else
      y[self$liesInSupport(x, F)] = private$.pdf(x[self$liesInSupport(x, F)], self = self)

    return(y)
  }
}) # NEEDS TESTING
Distribution$set("public","cdf",function(q, lower.tail = TRUE, log.p = FALSE){
  if(is.null(private$.cdf(1)))
    return(NULL)

  if(self$liesInSupport(q)){
      return(private$.cdf(q, self = self, lower.tail = TRUE, log.p = FALSE))
  }else
    warning(sprintf("%s does not lie in the support of %s",x,getR6Class(self)))
}) # NEEDS TESTING
Distribution$set("public","quantile",function(p, lower.tail = TRUE, log.p = FALSE){
  if(is.null(private$.quantile(1)))
    return(NULL)

  checkmate::assertNumeric(p, lower = 0, upper = 1)
  return(private$.quantile(p, self = self, lower.tail = TRUE, log.p = FALSE))
}) # NEEDS TESTING
Distribution$set("public","rand",function(n){
  if(is.null(private$.rand(1)))
    return(NULL)

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

Distribution$set("private",".parameters",data.frame())
Distribution$set("private",".pdf",NULL) # DONE
Distribution$set("private",".cdf",NULL) # DONE
Distribution$set("private",".rand",NULL) # DONE
Distribution$set("private",".quantile",NULL) # DONE
Distribution$set("private",".workingSupport",NULL) # DONE

