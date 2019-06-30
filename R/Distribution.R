#' @include Distribution_helpers.R SetInterval_operations.R
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
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
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
#'   \code{dmax()}  \tab \code{\link{dmax}} \cr
#'   \code{dmin()} \tab \code{\link{dmin}} \cr
#'   \code{skewnessType()} \tab \code{\link{skewnessType}} \cr
#'   \code{kurtosisType()} \tab \code{\link{kurtosisType}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{p/d/q/r Methods} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n, simplify = TRUE)} \tab \code{\link{rand}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Parameter Methods} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(lst, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE)} \tab \code{\link{liesInType}} \cr
#'   \code{liesInDistrDomain(x, all = TRUE)} \tab \code{\link{liesInDistrDomain}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{strprint()} \tab \code{\link{strprint}} \cr
#'   \code{print()} \tab \code{\link[base]{print}} \cr
#'   \code{summary(full = T)} \tab \code{\link{summary.Distribution}} \cr
#'   \code{plot()} \tab Coming Soon. \cr
#'   \code{qqplot()} \tab Coming Soon. \cr
#'   }
#'
#'
#' @seealso See \code{\link{SetInterval}} and \code{\link{SpecialSet}} for details on Sets and
#' Intervals. See \code{\link{ParameterSet}} for parameter details. See
#' \code{\link{DistributionDecorator}} for Decorator details.
#'
#' @export
NULL
#-------------------------------------------------------------
# Distribution Definition
#-------------------------------------------------------------
Distribution <- R6::R6Class("Distribution", lock_objects = FALSE)


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

  if(getR6Class(self) == "Distribution" | inherits(self,"DistributionWrapper")){

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
      private$.isPdf <- TRUE
    } else
      private$.pdf <- function(...) return(NULL)

    if(!is.null(cdf)){
      if(!is.null(formals(cdf)$self))
        formals(cdf)$self = self
      else
        formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
      private$.cdf <- cdf
      private$.isCdf <- TRUE
    } else
      private$.cdf <- function(...) return(NULL)

    if(!is.null(pdf) & !is.null(cdf)){
      checkmate::assert(length(formals(pdf)) == length(formals(cdf)),
                        .var.name = "'pdf' and 'cdf' must take the same arguments.")
      checkmate::assert(all(names(formals(pdf)) == names(formals(cdf))),
                        .var.name = "'pdf' and 'cdf' must take the same arguments.")
    }


    if(!is.null(quantile)){
      if(!is.null(formals(quantile)$self))
        formals(quantile)$self = self
      else
        formals(quantile) = c(formals(quantile),list(self=self),alist(...=))
      private$.quantile <- quantile
      private$.isQuantile <- TRUE
    } else
      private$.quantile <- function(...) NULL

    if(!is.null(rand)){
      if(!is.null(formals(rand)$self))
        formals(rand)$self = self
      else
        formals(rand) = c(formals(rand),list(self=self),alist(...=))
      private$.rand <- rand
      private$.isRand <- TRUE
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

    if(!is.null(pdf)){
      private$.pdf <- pdf
      private$.isPdf <- TRUE
    }
    if(!is.null(cdf)){
      private$.cdf <- cdf
      private$.isCdf <- TRUE
    }
    if(!is.null(quantile)){
      private$.quantile <- quantile
      private$.isQuantile <- TRUE
    }
    if(!is.null(rand)){
      private$.rand <- rand
      private$.isRand <- TRUE
    }

    if(!is.null(symmetry)){
      symm = ifelse(symmetric,"symmetric","asymmetric")
      private$.properties$symmetry <- symm
    }
    if(!is.null(support)) private$.properties$support <- support
    if(!is.null(distrDomain)) private$.properties$distrDomain <- distrDomain
    if(!is.null(description)) self$description <- description

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
#' @title String Representation of Print
#' @name strprint
#' @description Parsable string to be supplied to \code{print}, \code{data.frame}, etc.
#' @details strprint is a suggested method that should be included in all R6 classes to be passed to
#' methods such as \code{cat}, \code{summary} and \code{print}. Additionally can be used to easily
#' parse R6 objects into data-frames, see examples.
#'
#' @param object R6 object
#' @usage strprint(object)
#'
#' @export
Distribution$set("public","strprint",function(){
  if(length(private$.parameters)!=0){
    settable = self$parameters()$as.data.table()$settable
    id = self$parameters()$as.data.table()[settable, "id"]
    value = self$parameters()$as.data.table()[settable, "value"]
    string = paste0(self$short_name, "(", paste(id, value, sep = " = ", collapse = ", "), ")")
  } else {
    string = paste0(self$short_name)
  }
  return(string)
})
Distribution$set("public","print",function(...){
  cat(self$strprint())
  invisible(self)
})

#' @title Distribution Summary
#' @description Summary method for distribution objects (and all child classes).
#'
#' @section R6 Usage: $summary(full = TRUE)
#' @param object Distribution.
#' @param full logical; if TRUE (default), gives an extended summary, otherwise brief.
#' @param ... additional arguments.
#'
#' @seealso \code{\link{Distribution}}
#'
#' @export
summary.Distribution <- function(object, full = TRUE,...) {}
Distribution$set("public","summary",function(full = TRUE,...){

  which_params = self$parameters()$as.data.table()$settable

  if(full){
    if(length(private$.parameters)!=0){
      cat(self$name,"with parameterisation:\n")
      settable = self$parameters()$as.data.table()$settable
      cat("\t", paste(self$parameters()$as.data.table()[settable, "id"],
                      self$parameters()$as.data.table()[settable, "value"],
                      sep = " = ", collapse = ", "))
    } else
      cat(self$name(),"\n")
    cat("\n\n Quick Statistics: \n")

    a_exp = suppressMessages(try(self$mean(), silent = T))
    a_var = suppressMessages(try(self$var(), silent = T))
    a_skew = suppressMessages(try(self$skewness(), silent = T))
    a_kurt = suppressMessages(try(self$kurtosis(), silent = T))

    if(!inherits(a_exp,"try-error")) cat("\tMean")
    if(!inherits(a_var,"try-error")) cat("\tVariance")
    if(!inherits(a_skew,"try-error")) cat("\tSkewness")
    if(!inherits(a_kurt,"try-error")) cat("\tExcess Kurtosis")
    cat("\n")

    if(!inherits(a_exp,"try-error")){
      if(length(a_exp) > 1)
        cat("\t", paste0(a_exp, collapse = ", "), sep = "")
      else
        cat("\t", a_exp, sep = "")
    }
    if(!inherits(a_var,"try-error")){
      if(length(a_var) > 1)
        cat("\t", paste0(a_var, collapse = ", "), sep = "")
      else
        cat("\t", a_var, sep = "")
    }
    if(!inherits(a_skew,"try-error")){
      if(length(a_skew) > 1)
        cat("\t\t", paste0(a_skew, collapse = ", "), sep = "")
      else
        cat("\t\t", a_skew, sep = "")
    }
    if(!inherits(a_kurt,"try-error")){
      if(length(a_kurt) > 1)
        cat("\t\t", paste0(a_kurt, collapse = ", "), sep = "")
      else
        cat("\t\t", a_kurt, sep = "")
    }
    cat("\n\n")

    cat(" Support:",self$support()$getSymbol(), "\t Scientific Type:",self$type()$getSymbol(),"\n")
    cat("\n Traits: ",self$valueSupport(),"; ",self$variateForm(),"\n\t See traits() for more",sep="")

    if(inherits(a_kurt,"try-error"))
      cat("\n Properties: ", self$distrDomain()$getSymbol(), "; ", self$symmetry(),
          "\n\t See properties() for more", sep="")
    else
      cat("\n Properties: ", self$kurtosisType(), "; ", self$skewnessType(),"; ", self$symmetry(),
          "\n\t See properties() for more", sep="")

    if(length(self$decorators())!=0)
      cat("\n\n Decorated with: ", paste0(self$decorators(),collapse=", "))

  } else {
    if(length(private$.parameters)!=0)
      cat(self$strprint())
    else
      cat(self$name)
    cat("\n Scientific Type:",self$type()$getSymbol(),"\t See traits() for more")
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
#' @param object Distribution.
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
#' @param object Distribution.
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
#' @param object Distribution.
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
#' @param object Distribution.
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
#' @param object Distribution.
#' @description Returns the scientific type of the distribution.
#' @seealso \code{\link{SetInterval}}
#' @export
NULL
Distribution$set("public","properties",function(){
  return(private$.properties)
})

#' @name support
#' @title Support Accessor
#' @usage support(object)
#' @section R6 Usage: $support()
#' @param object Distribution.
#' @description Returns the support of the distribution.
#' @details The support of a probability distribution is defined as the interval where the pmf/pdf is
#' greater than zero,
#' \deqn{Supp(X) = \{ x \in R: f_X(x) > 0\}}
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
#' @param object Distribution.
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
#' @param object Distribution.
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
#' @param object Distribution.
#' @description Returns the distribution supremum as the supremum of the support.
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{dmin}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("public","sup",function(){
  return(self$support()$sup())
})

#' @name inf
#' @title Infimum Accessor
#' @usage inf(object)
#' @section R6 Usage: $inf()
#' @param object Distribution.
#' @description Returns the distribution infimum as the infimum of the support.
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{dmin}}, \code{\link{sup}}
#' @export
NULL
Distribution$set("public","inf",function(){
  return(self$support()$inf())
})

#' @name dmax
#' @title Distribution Maximum Accessor
#' @usage dmax(object)
#' @section R6 Usage: $dmax()
#' @param object Distribution.
#' @description Returns the distribution maximum as the maximum of the support. If the support is not
#' bounded above then maximum is given by
#' \deqn{maximum = supremum - 2.220446e-16}
#' @seealso \code{\link{support}}, \code{\link{dmin}}, \code{\link{sup}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("public","dmax",function(){
  return(self$support()$max())
})

#' @name dmin
#' @title Distribution Minimum Accessor
#' @usage dmin(object)
#' @section R6 Usage: $dmin()
#' @param object Distribution.
#' @description Returns the distribution minimum as the minimum of the support. If the support is not
#' bounded below then minimum is given by
#' \deqn{minimum = infimum + 2.220446e-16}
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{sup}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("public","dmin",function(){
  return(self$support()$min())
})

#' @name kurtosisType
#' @title Type of Kurtosis Accessor
#' @usage kurtosisType(object)
#' @section R6 Usage: $kurtosisType()
#' @param object Distribution.
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
#' @param object Distribution.
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

  if(length(lst)!=0){

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
  }
})

#-------------------------------------------------------------
# Public Methods - pdf
#-------------------------------------------------------------
#' @name pdf
#' @title Probability Density/Mass Function
#' @description Returns the probability density/mass function for continuous/discrete (or mixture)
#' distributions evaluated at a given point.
#'
#' @usage pdf(object, x1, ..., log = FALSE, simplify = TRUE)
#' @section R6 Usage: $pdf(x1, ..., log = FALSE, simplify = TRUE)
#' @param object Distribution.
#' @param x1 vector of numerics to evaluate function at.
#' @param ... additional arguments.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param simplify if TRUE (default) returns results in simplest form (vector or data.table) otherwise as data.table.
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
#' @seealso \code{\link{cdf}}, \code{\link{quantile}}, \code{\link{rand}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","pdf",function(x1, ..., log = FALSE, simplify = TRUE){

  if(!private$.isPdf)
    return(NULL)

  if(testUnivariate(self)){
    pdf = numeric(length(x1))
    if(any(self$liesInSupport(x1, all = F)))
      pdf[self$liesInSupport(x1, all = F)] = private$.pdf(x1[self$liesInSupport(x1, all = F)])
  } else {
    if(is.null(x1)) pdf = private$.pdf(...)
    else pdf = private$.pdf(x1, ...)
  }

  if(log) pdf <- log(pdf)

  if(inherits(pdf,"data.table"))
    return(pdf)
  else{
    if(simplify)
      return(pdf)
    else{
      pdf = data.table::data.table(pdf)
      colnames(pdf) = self$short_name
      return(pdf)
    }
  }
})
#-------------------------------------------------------------
# Public Methods - cdf
#-------------------------------------------------------------
#' @name cdf
#' @title Cumulative Distribution Function
#' @description Returns the cumulative distribution function for a distribution evaluated at a given
#' point.
#'
#' @usage cdf(object, x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)
#' @section R6 Usage: $cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)
#' @param object Distribution.
#' @param x1 vector of numerics to evaluate function at.
#' @param ... additional arguments.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P(X \le x)} otherwise, \eqn{P(X > x)}.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @param simplify if TRUE (default) returns results in simplest form (vector or data.table) otherwise as data.table.
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
#' @seealso \code{\link{pdf}}, \code{\link{quantile}}, \code{\link{rand}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","cdf",function(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE){

  if(!private$.isCdf)
    return(NULL)

  if(testUnivariate(self)){
    cdf = numeric(length(x1))
    cdf[x1 > self$sup()] = 1
    if(any(self$liesInSupport(x1, all = F)))
      cdf[self$liesInSupport(x1, all = F)] = private$.cdf(x1[self$liesInSupport(x1, all = F)])
  } else {
    if(is.null(x1)) cdf = private$.cdf(...)
    else cdf = private$.cdf(x1, ...)
  }

  if(log.p & lower.tail) cdf = log(cdf)
  else if(log.p & !lower.tail) cdf = log(1 - cdf)
  else if(!log.p & lower.tail) cdf = cdf
  else cdf = 1 - cdf

  if(inherits(cdf,"data.table"))
    return(cdf)
  else{
    if(simplify)
      return(cdf)
    else{
      cdf = data.table::data.table(cdf)
      colnames(cdf) = self$short_name
      return(cdf)
    }
  }



}) # NEEDS TESTING
#-------------------------------------------------------------
# Public Methods - quantile
#-------------------------------------------------------------
#' @title Inverse Cumulative Distribution Function
#' @description Returns the inverse cumulative distribution, aka quantile, function for a distribution
#' evaluated at a given point between 0 and 1.
#'
#' @importFrom stats quantile
#' @section R6 Usage: $quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)
#' @param x Distribution.
#' @param p vector of probabilities to evaluate function at.
#' @param ... additional arguments.
#' @param lower.tail logical; if TRUE, probabilities p are given as log(p).
#' @param log.p ignored, retained for consistency.
#' @param simplify if TRUE (default) returns results in simplest form (vector or data.table) otherwise as data.table.
#'
#' @details
#'  The quantile function, \eqn{q_X}, is the inverse cdf, i.e.
#'  \deqn{q_X(p) = F^{-1}_X(p) = \inf\{x \in R: F_X(x) \ge p\}}
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
#' @seealso \code{\link{pdf}}, \code{\link{cdf}}, \code{\link{rand}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
quantile.Distribution <- function(x, p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE) {}
Distribution$set("public","quantile",function(p, ..., lower.tail = TRUE, log.p, simplify = TRUE){

  if(!private$.isQuantile)
    return(NULL)

  if(!lower.tail)
    p = 1 - p

  quantile = p
  quantile[p > 1] = NaN
  quantile[p < 0] = NaN
  quantile[p == 0] = self$inf()
  quantile[p == 1] = self$sup()
  if(sum(p > 0 & p < 1)!=0)
    quantile[p > 0 & p < 1] = private$.quantile(quantile[p > 0 & p < 1])

  if(inherits(quantile,"data.table"))
    return(quantile)
  else{
    if(simplify)
      return(quantile)
    else{
      quantile = data.table::data.table(quantile)
      colnames(quantile) = self$short_name
      return(quantile)
    }
  }



}) # NEEDS TESTING
#-------------------------------------------------------------
# Public Methods - rand
#-------------------------------------------------------------
#' @name rand
#' @title Random Simulation Function
#' @description Returns a given number of points sampled from the distribution.
#'
#' @usage rand(object, n, simplify = TRUE)
#' @section R6 Usage: $rand(n, simplify = TRUE)
#' @param object Distribution.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param simplify if TRUE (default) returns results in simplest form (vector or data.table) otherwise as data.table.
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
#' @seealso \code{\link{pdf}}, \code{\link{cdf}}, \code{\link{quantile}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","rand",function(n, simplify = TRUE){

  if(!private$.isRand)
    return(NULL)

  if(length(n) > 1)
    n = length(n)

  rand = private$.rand(n)
  if(inherits(rand,"data.table"))
    return(rand)
  else{
    if(simplify)
      return(rand)
    else{
      rand = data.table::data.table(rand)
      colnames(rand) = self$short_name
      return(rand)
    }
  }

})

#-------------------------------------------------------------
# Public Methods - Analytic Maths/stats
#-------------------------------------------------------------
#' @name prec
#' @title Precision of a Distribution
#' @description Precision of a distribution assuming variance is provided.
#'
#' @usage prec(object)
#' @section R6 Usage: $prec()
#' @param object Distribution.
#' @details The precision is analytically computed as the reciprocal of the variance.
#' If the variance is not found in the distribution (analytically or numerically), returns error.
#'
#' @seealso \code{\link{var}}
#'
#' @export
NULL
Distribution$set("public","prec",function(){
  return(1/self$var())
})

#' @name sd
#' @title Standard Deviation of a Distribution
#' @description Standard deviation of a distribution assuming variance is provided.
#'
#' @usage sd(object)
#' @section R6 Usage: $sd()
#' @param object Distribution.
#' @details The standard deviation is analytically computed as the square root of the variance.
#' If the variance is not found in the distribution (analytically or numerically), returns error.
#'
#' @seealso \code{\link{var}}
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
#' @param x Distribution.
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

#' @title Distribution Interquartile Range
#' @name iqr
#' @description Interquartile range of a distribution
#'
#' @usage iqr(object)
#' @section R6 Usage: $iqr()
#'
#' @param object Distribution.
#'
#' @details The interquartile range of a distribution is defined by
#' \deqn{iqr_X = q(0.75) - q(0.25)}
#' where q is the quantile, or inverse distribution function.
#'
#' Returns error if the quantile function is missing.
#'
#' @export
NULL
Distribution$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})

#-------------------------------------------------------------
# Public Methods - Validation
#-------------------------------------------------------------
#' @name liesInSupport
#' @title Test if Data Lies in Distribution Support
#' @description Tests if the given data lies in the support of the Distribution, either tests if all
#' data lies in the support or any of it.
#'
#' @usage liesInSupport(object, x, all = TRUE, bound = FALSE)
#' @section R6 Usage: $liesInSupport(x, all = TRUE, bound = FALSE)
#' @param object Distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) uses dmin/dmax otherwise inf/sup.
#' @details If \code{all} is TRUE (default) returns TRUE only if every element in \code{x}
#' lies in the support. If \code{all} is FALSE then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInType}} and \code{\link{liesInDistrDomain}}
#'
#' @export
NULL
Distribution$set("public","liesInSupport",function(x, all = TRUE, bound = FALSE){
  return(self$support()$liesInSetInterval(x, all, bound))
})

#' @name liesInType
#' @title Test if Data Lies in Distribution Type
#' @description Tests if the given data lies in the type of the Distribution, either tests if all
#' data lies in the type or any of it.
#'
#' @usage liesInType(object, x, all = TRUE, bound = FALSE)
#' @section R6 Usage: $liesInType(x, all = TRUE, bound = FALSE)
#' @param object Distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) uses dmin/dmax otherwise inf/sup.
#' @details If \code{all} is \code{TRUE} (default) returns \code{TRUE} only if every element in \code{x}
#' lies in the type. If \code{all} is \code{FALSE} then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInSupport}} and \code{\link{liesInDistrDomain}}
#'
#' @export
NULL
Distribution$set("public","liesInType",function(x, all = TRUE, bound = FALSE){
  return(self$type()$liesInSetInterval(x, all, bound))
})

#' @name liesInDistrDomain
#' @title Test if Data Lies in Distribution Domain
#' @description Tests if the given data lies in the domain of the Distribution, either tests if all
#' data lies in the distribution domain or any of it.
#'
#' @usage liesInDistrDomain(object, x, all = TRUE, bound = FALSE)
#' @section R6 Usage: $liesInDistrDomain(x, all = TRUE, bound = FALSE)
#' @param object Distribution.
#' @param x vector of numerics to test.
#' @param all logical, see details.
#' @param bound logical, if FALSE (default) uses dmin/dmax otherwise inf/sup.
#' @details If \code{all} is \code{TRUE} (default) returns \code{TRUE} only if every element in \code{x}
#' lies in the domain. If \code{all} is \code{FALSE} then returns a vector of logicals for each corresponding element
#' in the vector \code{x}.
#'
#' @seealso \code{\link{liesInSupport}} and \code{\link{liesInType}}
#'
#' @export
NULL
Distribution$set("public","liesInDistrDomain",function(x, all = TRUE, bound = FALSE){
  return(self$distrDomain()$liesInSetInterval(x, all, bound))
})

#-------------------------------------------------------------
# Distribution Public Variables
#-------------------------------------------------------------
Distribution$set("public","name",character(0))
Distribution$set("public","short_name",character(0))
Distribution$set("public","description",NULL)
Distribution$set("public","traits",list())
Distribution$set("private",".pdf", NULL)
Distribution$set("private",".cdf", NULL)
Distribution$set("private",".quantile", NULL)
Distribution$set("private",".rand", NULL)
Distribution$set("private",".parameters",data.frame())
Distribution$set("private",".workingSupport",NULL)
Distribution$set("private",".decorators", NULL)
Distribution$set("private",".properties",NULL)
Distribution$set("private",".isPdf", FALSE)
Distribution$set("private",".isCdf", FALSE)
Distribution$set("private",".isQuantile", FALSE)
Distribution$set("private",".isRand", FALSE)

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
