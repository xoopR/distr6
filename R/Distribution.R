#' @include Distribution_helpers.R SetInterval_helpers.R
#-------------------------------------------------------------
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
#' }
#'
#' @section Constructor Details:
#'
#'   The most basic Distribution object consists of a name and one of pdf/cdf.
#'
#'   If supplied, \code{type}, \code{support} and \code{distrDomain} should be given as an R6 SetInterval
#'   object. If none are supplied then the set of Reals is taken to be the type and the dimension is the
#'   number of formal arguments in the pdf/cdf. If only \code{type} is supplied then this is taken to also
#'   be the support and domain.
#'
#'   By default, missing \code{pdf}, \code{cdf}, \code{quantile} and \code{rand} are not automatically imputed.
#'   Use the \code{\link{FunctionImputation}} decorator to generate these.
#'
#'   See \code{\link{ParameterSet}} for more details on construction of a ParameterSet.
#'
#'   \code{decorators} is an optional list of decorators (R6 environments not strings) to decorate the
#'   Distribution in construction. Decorators can also be added after construction. See
#'   \code{\link{DistributionDecorator}} and \code{\link{decorate}} for more details.
#'
#'   \code{valueSupport} should be one of continuous/discrete/mixture if supplied.
#'   \code{variateForm} should be one of univariate/multivariate/matrixvariate if supplied.
#'   If not given these are automatically filled from \code{type} and \code{support}.
#'
#' @section Public Variables:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Return} \cr
#'   \code{name} \tab Name of distribution. \cr
#'   \code{short_name} \tab Id of distribution. \cr
#'   \code{description} \tab Brief description of distribution. \cr
#'   \code{traits} \tab List: type, valueSupport, variateForm.
#'   }
#'
#' @section Accessor Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{decorators()} \tab \code{\link{decorators}} \cr
#'   \code{valueSupport()} \tab \code{\link{valueSupport}} \cr
#'   \code{variateForm()} \tab \code{\link{variateForm}} \cr
#'   \code{type()} \tab \code{\link{type}} \cr
#'   \code{properties()} \tab \code{\link{properties}} \cr
#'   \code{support()} \tab \code{\link{support}} \cr
#'   \code{distrDomain()} \tab \code{\link{distrDomain}} \cr
#'   \code{symmetry()} \tab \code{\link{symmetry}} \cr
#'   \code{sup()}  \tab \code{\link{sup}} \cr
#'   \code{inf()} \tab \code{\link{inf}} \cr
#'   \code{skewnessType()} \tab \code{\link{skewnessType}} \cr
#'   \code{kurtosisType()} \tab \code{\link{kurtosisType}} \cr
#'   }
#'
#' @section Math/Stats Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n)} \tab \code{\link{rand}} \cr
#'   \code{sd()} \tab \code{\link{sd}} \cr
#'   \code{median()} \tab \code{link{median.Distribution}} \cr
#'  }
#'
#' @section Parameter Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(lst, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#' }
#'
#' @section Validation Methods:
#'  \tabular{ll}{
#'   \strong{Method} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE)} \tab \code{\link{liesInType}} \cr
#'   \code{liesInDistrDomain(x, all = TRUE)} \tab \code{\link{liesInDistrDomain}} \cr
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
#' @seealso See \code{\link{SetInterval}} and \code{\link{SpecialSet}} for details on Sets and
#' Intervals. See \code{\link{ParameterSet}} for parameter details. See
#' \code{\link{DistributionDecorator}} for Decorator details.
NULL
#-------------------------------------------------------------
#-------------------------------------------------------------
# Distribution Definition
#-------------------------------------------------------------
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
})
Distribution$set("private",".getWorkingSupportRange",function(){
  return(private$.workingSupport$inf:private$.workingSupport$sup)
})
Distribution$set("private",".updateDecorators", function(decs){
  private$.decorators <- decs
})

#-------------------------------------------------------------
# Public Methods - Constructor
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name = NULL, short_name = NULL,
                      type = NULL, support = NULL, distrDomain = NULL,
                      symmetric = logical(0),
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters = NULL, decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL
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
      rm = c("...","self")
      if(!is.null(pdf)){
        lng = length(formals(pdf)[!(names(formals(pdf)) %in% rm)])
        type <- Reals$new(dim = lng)
      } else{
        lng = length(formals(cdf)[!(names(formals(cdf)) %in% rm)])
        type <- Reals$new(dim = length(formals(cdf)))
      }
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

    self$traits$type <- type
    self$traits$valueSupport <- valueSupport
    self$traits$variateForm <- variateForm

    private$.properties$support <- support
    private$.properties$distrDomain <- distrDomain
    symm = ifelse(symmetric,"symmetric","asymmetric")
    private$.properties$symmetry <- symm

    if(!is.null(pdf)){
      if(!is.null(formals(pdf)$self))
        formals(pdf)$self = self
      else
        formals(pdf) = c(formals(pdf),list(self=self),alist(...=))
      private$.pdf <- pdf
    } else
      private$.pdf <- function(...) return(NULL)

    if(!is.null(cdf)){
      if(!is.null(formals(cdf)$self))
        formals(cdf)$self = self
      else
        formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
      private$.cdf <- cdf
    } else
      private$.cdf <- function(...) return(NULL)

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
      private$.quantile <- quantile
    } else
      private$.quantile <- function(...) NULL

    if(!is.null(rand)){
      if(!is.null(formals(rand)$self))
        formals(rand)$self = self
      else
        formals(rand) = c(formals(rand),list(self=self),alist(...=))
      private$.rand <- rand
    } else
      private$.rand <- function(...) NULL

    if(!is.null(parameters)){
      checkmate::assertClass(parameters,"ParameterSet")
      if(!inherits(self, "DistributionWrapper"))
        private$.parameters <- parameters$clone()$update()
      else
        private$.parameters <- parameters$clone()
    }
  }

    if(!is.null(decorators))
      suppressMessages(decorate(self, decorators))

    if(!is.null(pdf)) private$.pdf <- pdf
    if(!is.null(cdf)) private$.cdf <- cdf
    if(!is.null(quantile)) private$.quantile <- quantile
    if(!is.null(rand)) private$.rand <- rand


    if(!is.null(symmetry)){
      symm = ifelse(symmetric,"symmetric","asymmetric")
      private$.properties$symmetry <- symm
    }
    if(!is.null(support)) private$.properties$support <- support
    if(!is.null(distrDomain)) private$.properties$distrDomain <- distrDomain

    # Update skewness and kurtosis
    x = try(self$kurtosis(excess = TRUE), silent = TRUE)
    if(class(x) == "try-error")
      private$.properties$kurtosis <- NULL
    else
      private$.properties$kurtosis <- exkurtosisType(x)

    x = try(self$skewness(), silent = TRUE)
    if(class(x) == "try-error")
      private$.properties$skewness <- NULL
    else
      private$.properties$skewness <- skewType(x)

    # private$.setWorkingSupport()
    lockBinding("name",self)
    lockBinding("short_name",self)
    lockBinding("description",self)
    lockBinding("traits",self)
    lockBinding("parameters",self)

  invisible(self)
})

#-------------------------------------------------------------
# Public Methods - Representation
#-------------------------------------------------------------
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
})
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
})
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
          "\n\t See properties() for more", sep="")
    else
      cat("\n Properties: ", self$kurtosisType(), "; ", self$skewnessType(),"; ", self$symmetry(),
          "\n\t See properties() for more", sep="")

    if(length(self$decorators())!=0)
      cat("\n\n Decorated with: ", paste0(self$decorators(),collapse=", "))

  } else {
    if(length(private$.parameters)!=0){
      cat(self$short_name,"distribution with parameterisation: ")
      cat(paste(self$parameters()$as.data.frame()[which_params,"id"][[1]],
                self$parameters()$as.data.frame()[which_params,"value"][[1]],
                sep = " = ", collapse = "; "))
    } else
        cat(self$name)
    cat("\n Scientific Type:",self$type()$getSymbol(),"\t See traits for more")
    cat("\n Support:",self$support()$getSymbol(),"\t\t See properties() for more")
  }
})
Distribution$set("public","plot",function(){}) # TO DO
Distribution$set("public","qqplot",function(){}) # TO DO

#-------------------------------------------------------------
# Public Methods - Accessors
#-------------------------------------------------------------
#' @name decorators
#' @title Decorators Accessor
#' @usage decorators(object)
#' @section R6 Usage: $decorators()
#' @param object distribution.
#' @description Returns the decorators added to a distribution.
#' @seealso \code{\link{decorate}} and \code{\link{DistributionDecorator}}
#' @export
NULL
Distribution$set("public","decorators", function(){
  return(private$.decorators)
})

#' @name valueSupport
#' @title Value Support Accessor
#' @usage valueSupport(object)
#' @section R6 Usage: $valueSupport()
#' @param object distribution.
#' @description Returns the valueSupport of the distribution, one of discrete/continuous/mixture.
#' @export
NULL
Distribution$set("public","valueSupport",function(){
  return(self$traits[["valueSupport"]])
})

#' @name variateForm
#' @title Variate Form Accessor
#' @usage variateForm(object)
#' @section R6 Usage: $variateForm()
#' @param object distribution.
#' @description Returns the variateForm of the distribution, one of univariate/multivariate/matrixvariate.
#' @export
NULL
Distribution$set("public","variateForm",function(){
  return(self$traits[["variateForm"]])
})

#' @name type
#' @title Type Accessor
#' @usage type(object)
#' @section R6 Usage: $type()
#' @param object distribution.
#' @description Returns the scientific type of the distribution.
#' @seealso \code{\link{SetInterval}}
#' @export
NULL
Distribution$set("public","type",function(){
  return(self$traits[["type"]])
})

#' @name properties
#' @title Properties Accessor
#' @usage properties(object)
#' @section R6 Usage: $properties()
#' @param object distribution.
#' @description Returns the scientific type of the distribution.
#' @seealso \code{\link{SetInterval}}
#' @export
NULL
Distribution$set("public","properties",function() return(private$.properties))

#' @name support
#' @title Support Accessor
#' @usage support(object)
#' @section R6 Usage: $support()
#' @param object distribution.
#' @description Returns the support of the distribution.
#' @details The support of a probability distribution is defined as the interval where the pmf/pdf is
#' greater than zero,
#' \deqn{Supp(X) = \{ x \in \mathbb{R}: f_X(x) > 0\}}
#' where f_X is the pmf if distribution X is discrete, otherwise the pdf.
#' @seealso \code{\link{SetInterval}} and \code{\link{properties}}
#' @export
NULL
Distribution$set("public","support",function(){
  return(self$properties()[["support"]])
})

#' @name distrDomain
#' @title Distribution Domain Accessor
#' @usage distrDomain(object)
#' @section R6 Usage: $distrDomain()
#' @param object distribution.
#' @description Returns the distribution domain.
#' @details The domain of a probability distribution is the set of values returned by the pdf/pdf,
#' including zero.
#' @seealso \code{\link{SetInterval}} and \code{\link{properties}}
#' @export
NULL
Distribution$set("public","distrDomain",function(){
  return(self$properties()[["distrDomain"]])
})

#' @name symmetry
#' @title Symmetry Accessor
#' @usage symmetry(object)
#' @section R6 Usage: $symmetry()
#' @param object distribution.
#' @description Returns the distribution symmetry, one of "symmetric" or "asymmetric".
#' @seealso \code{\link{properties}}
#' @export
NULL
Distribution$set("public","symmetry",function(){
  return(self$properties()[["symmetry"]])
})

#' @name sup
#' @title Supremum Accessor
#' @usage sup(object)
#' @section R6 Usage: $sup()
#' @param object distribution.
#' @description Returns the distribution supremum as the supremum of the support.
#' @seealso \code{\link{support}} and \code{\link{inf}}
#' @export
NULL
Distribution$set("public","sup",function(){
  return(self$support()$sup())
})

#' @name inf
#' @title Infimum Accessor
#' @usage inf(object)
#' @section R6 Usage: $inf()
#' @param object distribution.
#' @description Returns the distribution infimum as the infimum of the support.
#' @seealso \code{\link{support}} and \code{\link{sup}}
#' @export
NULL
Distribution$set("public","inf",function(){
  return(self$support()$inf())
})

#' @name kurtosisType
#' @title Type of Kurtosis Accessor
#' @usage kurtosisType(object)
#' @section R6 Usage: $kurtosisType()
#' @param object distribution.
#' @description If the distribution kurtosis is present in properties, returns the type of kurtosis
#' (platykurtic/mesokurtic/leptokurtic), otherwise returns NULL.
#' @seealso \code{\link{kurtosis}}, \code{\link{properties}} and \code{\link{skewnessType}}
#' @export
NULL
Distribution$set("public", "kurtosisType", function() {
  x = self$properties()$kurtosis
  if(inherits(x,"try-error"))
    return(NA)
  else
    return(x)
})

#' @name skewnessType
#' @title Type of Skewness Accessor
#' @usage skewnessType(object)
#' @section R6 Usage: $skewnessType()
#' @param object distribution.
#' @description If the distribution skewness is present in properties, returns the type of skewness
#' (negative/none/positive), otherwise returns NULL.
#' @seealso \code{\link{skewness}}, \code{\link{properties}} and \code{\link{kurtosisType}}
#' @export
NULL
Distribution$set("public", "skewnessType", function() {
  x = self$properties()$skewness
  if(inherits(x,"try-error"))
    return(NA)
  else
    return(x)
})

#-------------------------------------------------------------
# Public Methods - Parameters
#-------------------------------------------------------------
# Documented in ParameterSet.R
Distribution$set("public","parameters",function(id = NULL, error = "warn"){
  if(length(private$.parameters)==0)
    return(NULL)
  else
    return(private$.parameters$parameters(id, error))
})
# Documented in ParameterSet.R
Distribution$set("public","getParameterValue",function(id, error = "warn"){
  return(private$.parameters$getParameterValue(id, error))
})
# Documented in ParameterSet.R
Distribution$set("public","setParameterValue",function(lst, error = "warn"){

  self$parameters()$setParameterValue(lst, error)

  # Update skewness and kurtosis
  x = try(self$kurtosis(excess = TRUE), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$kurtosis <- NULL
  else
    private$.properties$kurtosis <- exkurtosisType(x)

  x = try(self$skewness(), silent = TRUE)
  if(class(x) == "try-error")
    private$.properties$skewness <- NULL
  else
    private$.properties$skewness <- skewType(x)

  #private$.setWorkingSupport()
  invisible(self)
})

#-------------------------------------------------------------
# Public Methods - pdf
#-------------------------------------------------------------
#' @name pdf
#' @title Probability Density/Mass Function
#' @description Returns the probability density/mass function for continuous/discrete (or mixture)
#' distributions evaluated at a given point.
#'
#' @usage pdf(object, x1, ..., log = FALSE)
#' @section R6 Usage: $pdf(x1, ..., log = FALSE)
#' @param object distribution.
#' @param x1 vector of numerics to evaluate function at.
#' @param ... additional arguments.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#'
#' @details
#'  For discrete distributions the probability mass function (pmf) is returned, defined as
#'  \deqn{p_X(x) = P(X = x)}
#'  for continuous distributions the probability density function (pdf), \eqn{f_X}, is returned
#'  \deqn{f_X(x) = P(x < X \le x + dx)}
#'  for some infinitesimally small \eqn{dx}.
#'
#' If available a pdf will be returned without warning using an analytic expression. Otherwise,
#' if the distribution has not been decorated with \code{FunctionImputation}, \code{NULL} is returned.
#' To impute the pdf, use \code{decorate(distribution, FunctionImputation)}, this will provide a numeric
#' calculation for the pdf with warning.
#'
#' Additional named arguments can be passed, which are required for composite distributions such as
#' \code{\link{ProductDistribution}} and \code{\link{ArrayDistribution}}.
#'
#' @seealso \code{\link{cdf}}, \code{\link{quantile}}, \code{\link{rand}} for other statistial functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","pdf",function(x1, ..., log = FALSE){
  if(testUnivariate(self)){
    pdf = x1
    pdf[!self$liesInSupport(x1, all = F)] = 0

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
#-------------------------------------------------------------
# Public Methods - cdf
#-------------------------------------------------------------
#' @name cdf
#' @title Cumulative Distribution Function
#' @description Returns the cumulative distribution function for a distribution evaluated at a given
#' point.
#'
#' @usage cdf(object, x1, ..., lower.tail = TRUE, log.p = FALSE)
#' @section R6 Usage: $cdf(x1, ..., lower.tail = TRUE, log.p = FALSE)
#' @param object distribution.
#' @param x1 vector of numerics to evaluate function at.
#' @param ... additional arguments.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P(X \le x)} otherwise, \eqn{P(X > x)}.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#'
#' @details
#'  The (lower tail) cumulative distribution function, \eqn{F_X}, is defined as
#'  \deqn{F_X(x) = P(X \le x)}
#'  If \code{lower.tail} is FALSE then \eqn{1 - F_X(x)} is returned, also known as the
#'  \code{\link{survival}} function.
#'
#' If available a cdf will be returned without warning using an analytic expression. Otherwise,
#' if the distribution has not been decorated with \code{FunctionImputation}, \code{NULL} is returned.
#' To impute the cdf, use \code{decorate(distribution, FunctionImputation)}, this will provide a numeric
#' calculation for the cdf with warning.
#'
#' Additional named arguments can be passed, which are required for composite distributions such as
#' \code{\link{ProductDistribution}} and \code{\link{ArrayDistribution}}.
#'
#' @seealso \code{\link{pdf}}, \code{\link{quantile}}, \code{\link{rand}} for other statistial functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","cdf",function(x1, ..., lower.tail = TRUE, log.p = FALSE){

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
#-------------------------------------------------------------
# Public Methods - quantile
#-------------------------------------------------------------
#' @title Inverse Cumulative Distribution Function
#' @description Returns the inverse cumulative distribution, aka quantile, function for a distribution
#' evaluated at a given point between 0 and 1.
#'
#' @importFrom stats quantile
#' @section R6 Usage: $quantile(p, ..., lower.tail = TRUE, log.p = FALSE)
#' @param x distribution.
#' @param p vector of probabilities to evaluate function at.
#' @param ... additional arguments.
#' @param lower.tail logical; if TRUE, probabilities p are given as log(p).
#' @param log.p ignored, retained for consistency.
#'
#' @details
#'  The quantile function, \eqn{q_X}, is the inverse cdf, i.e.
#'  \deqn{q_X(p) = F^{-1}_X(p) = \inf\{x \in \mathbb{R}: F_X(x) \ge p\}}
#'
#'  If \code{lower.tail} is FALSE then \eqn{q_X(1-p)} is returned.
#'
#' If available a quantile will be returned without warning using an analytic expression. Otherwise,
#' if the distribution has not been decorated with \code{FunctionImputation}, \code{NULL} is returned.
#' To impute the quantile, use \code{decorate(distribution, FunctionImputation)}, this will provide a numeric
#' calculation for the quantile with warning.
#'
#' Additional named arguments can be passed, which are required for composite distributions such as
#' \code{\link{ProductDistribution}} and \code{\link{ArrayDistribution}}.
#'
#' @seealso \code{\link{pdf}}, \code{\link{cdf}}, \code{\link{rand}} for other statistial functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
quantile.Distribution <- function(x, p, ..., lower.tail = TRUE, log.p = FALSE) {}
Distribution$set("public","quantile",function(p, ..., lower.tail = TRUE, log.p){
    if(lower.tail){
      return(unlist(sapply(p, function(p0) private$.quantile(p0,...))))
    } else{
      return(unlist(sapply(p, function(p0) private$.quantile(1 - p0,...))))
    }
}) # NEEDS TESTING
#-------------------------------------------------------------
# Public Methods - rand
#-------------------------------------------------------------
#' @name rand
#' @title Random Simulation Function
#' @description Returns a given number of points sampled from the distribution.
#'
#' @usage rand(object, n)
#' @section R6 Usage: $rand(n)
#' @param object distribution.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#'
#' @details
#' If available a rand will be returned without warning using an analytic expression. Otherwise,
#' if the distribution has not been decorated with \code{FunctionImputation}, \code{NULL} is returned.
#' To impute the rand, use \code{decorate(distribution, FunctionImputation)}, this will provide a numeric
#' calculation for the rand with warning.
#'
#' Additional named arguments can be passed, which are required for composite distributions such as
#' \code{\link{ProductDistribution}} and \code{\link{ArrayDistribution}}.
#'
#' @seealso \code{\link{pdf}}, \code{\link{cdf}}, \code{\link{quantile}} for other statistial functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","rand",function(n){
  if(length(n) > 1)
    n = length(n)

  return(private$.rand(n))
})

#-------------------------------------------------------------
# Public Methods - Analytic Maths/stats
#-------------------------------------------------------------
#' @name sd
#' @title Standard Deviation of a Distribution
#' @description Standard deviation of a distribution assuming variance is provided.
#'
#' @usage sd(object)
#' @section R6 Usage: $sd()
#' @param object distribution.
#' @details The standard deviation is analytically computed as the square root of the variance.
#' If the variance is not found in the distribution (analytically or numerically), returns error.
#'
#' @seealso \code{\link{var.Distribution}}
#'
#' @export
NULL
Distribution$set("public","sd",function(){
  return(sqrt(self$var()))
})

#' @title Median of a Distribution
#' @description Median of a distribution assuming quantile is provided.
#'
#' @importFrom stats median
#' @section R6 Usage: $median()
#' @param x distribution.
#' @param na.rm ignored, added for consistency with S3 generic.
#' @param ... ignored, added for consistency with S3 generic.
#' @details The median is computed as the quantile function evaluated at 0.5.
#' If the quantile is not found in the distribution (analytically or numerically), returns error.
#'
#' @seealso \code{\link{quantile.Distribution}}
#'
#' @export
median.Distribution <- function(x, na.rm = NULL, ..) {}
Distribution$set("public","median",function(na.rm = NULL,...){
  return(self$quantile(0.5))
})

#-------------------------------------------------------------
# Public Methods - Validation
#-------------------------------------------------------------
#' @name liesInSupport
#' @title Test if Data Lies in Distribution Support
#' @description Tests if the given data lies in the support of the Distribution, either tests if all
#' data lies in the support or any of it.
#'
#' @usage liesInSupport(object, x, all = TRUE)
#' @section R6 Usage: $liesInSupport(x, all = TRUE)
#' @param object distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @details If \code{all} is \code{TRUE} (default) returns \code{TRUE} only if every element in \code{x}
#' lies in the support. If \code{all} is \code{FALSE} then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInType}} and \code{\link{liesInDistrDomain}}
#'
#' @export
NULL
Distribution$set("public","liesInSupport",function(x, all = TRUE){
  if(all)
    return(all(x >= self$inf()) & all(x <= self$sup()))
  else
    return(x >= self$inf() & x <= self$sup())
})

#' @name liesInType
#' @title Test if Data Lies in Distribution Type
#' @description Tests if the given data lies in the type of the Distribution, either tests if all
#' data lies in the type or any of it.
#'
#' @usage liesInType(object, x, all = TRUE)
#' @section R6 Usage: $liesInType(x, all = TRUE)
#' @param object distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @details If \code{all} is \code{TRUE} (default) returns \code{TRUE} only if every element in \code{x}
#' lies in the type. If \code{all} is \code{FALSE} then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInSupport}} and \code{\link{liesInDistrDomain}}
#'
#' @export
NULL
Distribution$set("public","liesInType",function(x, all = TRUE){
  if(all)
    return(all(x >= self$type()$lower()) & all(x <= self$type()$upper))
  else
    return(x >= self$type()$lower() & x <= self$type()$upper)
})

#' @name liesInDistrDomain
#' @title Test if Data Lies in Distribution Domain
#' @description Tests if the given data lies in the domain of the Distribution, either tests if all
#' data lies in the distribution domain or any of it.
#'
#' @usage liesInDistrDomain(object, x, all = TRUE)
#' @section R6 Usage: $liesInDistrDomain(x, all = TRUE)
#' @param object distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @details If \code{all} is \code{TRUE} (default) returns \code{TRUE} only if every element in \code{x}
#' lies in the domain. If \code{all} is \code{FALSE} then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInSupport}} and \code{\link{liesInType}}
#'
#' @export
NULL
Distribution$set("public","liesInDistrDomain",function(x, all = TRUE){
  if(all)
    return(all(x >= self$distrDomain()$lower()) & all(x <= self$distrDomain()$upper))
  else
    return(x >= self$distrDomain()$lower() & x <= self$distrDomain()$upper)
})

#-------------------------------------------------------------
# Distribution Public Variables
#-------------------------------------------------------------
Distribution$set("public","name",character(0))
Distribution$set("public","short_name",character(0))
Distribution$set("public","description",NULL)
Distribution$set("public","traits",list())
#-------------------------------------------------------------
# Distribution Private Variables
#-------------------------------------------------------------
Distribution$set("private",".pdf", NULL)
Distribution$set("private",".cdf", NULL)
Distribution$set("private",".quantile", NULL)
Distribution$set("private",".rand", NULL)
Distribution$set("private",".parameters",data.frame())
Distribution$set("private",".workingSupport",NULL)
Distribution$set("private",".decorators", NULL)
Distribution$set("private",".properties",NULL)
