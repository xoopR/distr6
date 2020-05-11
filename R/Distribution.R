#' @include distr6_globals.R helpers.R
#-------------------------------------------------------------
# Distribution Documentation
#-------------------------------------------------------------
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions.
#'
#' @name Distribution
#'
#' @section Constructor: Distribution$new(name = NULL, short_name = NULL, type = NULL, support = NULL,
#' symmetric = FALSE, pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
#' parameters = NULL, decorators = NULL, valueSupport = NULL, variateForm = NULL, description = NULL,
#' suppressMoments = TRUE)
#'
#' @section Constructor Arguments:
#' \tabular{lll}{
#' \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#' \code{name} \tab character \tab Full name of distribution. \cr
#' \code{short_name} \tab character \tab Short name to identify distribution. \cr
#' \code{type} \tab [set6::Set] \tab Scientific type. \cr
#' \code{support} \tab [set6::Set] \tab Distribution support. See Details. \cr
#' \code{symmetric} \tab logical \tab Is distribution symmetric? \cr
#' \code{pdf} \tab function \tab See Details. \cr
#' \code{cdf} \tab function \tab See Details. \cr
#' \code{quantile} \tab function \tab See Details. \cr
#' \code{rand} \tab function \tab See Details. \cr
#' \code{parameters} \tab ParameterSet \tab See Details. \cr
#' \code{decorators} \tab list \tab R6 decorators to add in construction. \cr
#' \code{valueSupport} \tab character \tab continuous, discrete, mixture. See Details. \cr
#' \code{variateForm} \tab character \tab univariate, multivariate, matrixvariate. See Details. \cr
#' \code{description} \tab character \tab Short description of distribution. \cr
#' \code{suppressMoments} \tab character \tab See Details. \cr
#' }
#'
#' @section Constructor Details:
#'
#'   The most basic Distribution object consists of a name and one of pdf/cdf.
#'
#'   If supplied, \code{type} and \code{support} should be given as a [set6::Set] object. If neither are supplied
#'   then the set of Reals is taken to be the type and the dimension is the number of formal arguments in the pdf/cdf.
#'   If only \code{type} is supplied then this is taken to also be the support.
#'
#'   By default, missing \code{pdf}, \code{cdf}, \code{quantile} and \code{rand} are not automatically imputed.
#'   Use the \code{\link{FunctionImputation}} decorator to generate these.
#'
#'   See \code{\link{ParameterSet}} for more details on construction of a ParameterSet.
#'
#'   \code{decorators} is an optional list of decorators (R6 environments not strings) to decorate the
#'   Distribution in construction. Decorators can also be added after construction. See \code{\link{decorate}}
#'   for more details.
#'
#'   \code{valueSupport} should be one of continuous/discrete/mixture if supplied.
#'   \code{variateForm} should be one of univariate/multivariate/matrixvariate if supplied.
#'   If not given these are automatically filled from \code{type} and \code{support}.
#'
#'   \code{suppressMoments} can be used to prevent the skewness and kurtosis type being automatically
#'   calculated in construction. This has the benefit of drastically decreasing computational time but
#'   at the cost of losing these in the distribution properties.
#'
#' @section Public Variables:
#'  \tabular{ll}{
#'   \strong{Variable} \tab \strong{Return} \cr
#'   \code{name} \tab Name of distribution. \cr
#'   \code{short_name} \tab Id of distribution. \cr
#'   \code{description} \tab Brief description of distribution. \cr
#'   }
#'
#' @section Public Methods:
#'  \tabular{ll}{
#'   \strong{Accessor Methods} \tab \strong{Link} \cr
#'   \code{decorators} \tab \code{\link{decorators}} \cr
#'   \code{traits} \tab \code{\link{traits}} \cr
#'   \code{valueSupport} \tab \code{\link{valueSupport}} \cr
#'   \code{variateForm} \tab \code{\link{variateForm}} \cr
#'   \code{type} \tab \code{\link{type}} \cr
#'   \code{properties} \tab \code{\link{properties}} \cr
#'   \code{support} \tab \code{\link{support}} \cr
#'   \code{symmetry} \tab \code{\link{symmetry}} \cr
#'   \code{sup}  \tab \code{\link{sup}} \cr
#'   \code{inf} \tab \code{\link{inf}} \cr
#'   \code{dmax}  \tab \code{\link{dmax}} \cr
#'   \code{dmin} \tab \code{\link{dmin}} \cr
#'   \code{skewnessType} \tab \code{\link{skewnessType}} \cr
#'   \code{kurtosisType} \tab \code{\link{kurtosisType}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{d/p/q/r Methods} \tab \strong{Link} \cr
#'   \code{pdf(x1, ..., log = FALSE, simplify = TRUE)} \tab \code{\link{pdf}} \cr
#'   \code{cdf(x1, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{cdf}}\cr
#'   \code{quantile(p, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE)} \tab \code{\link{quantile.Distribution}} \cr
#'   \code{rand(n, simplify = TRUE)} \tab \code{\link{rand}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Statistical Methods} \tab \strong{Link} \cr
#'   \code{prec()} \tab \code{\link{prec}} \cr
#'   \code{stdev()} \tab \code{\link{stdev}}\cr
#'   \code{median()} \tab \code{\link{median.Distribution}} \cr
#'   \code{iqr()} \tab \code{\link{iqr}} \cr
#'   \code{correlation()} \tab \code{\link{correlation}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Parameter Methods} \tab \strong{Link} \cr
#'   \code{parameters(id)} \tab \code{\link{parameters}} \cr
#'   \code{getParameterValue(id, error = "warn")}  \tab \code{\link{getParameterValue}} \cr
#'   \code{setParameterValue(..., lst = NULL, error = "warn")} \tab \code{\link{setParameterValue}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Validation Methods} \tab \strong{Link} \cr
#'   \code{liesInSupport(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInSupport}} \cr
#'   \code{liesInType(x, all = TRUE, bound = FALSE)} \tab \code{\link{liesInType}} \cr
#'   \tab \cr \tab \cr \tab \cr
#'   \strong{Representation Methods} \tab \strong{Link} \cr
#'   \code{strprint(n = 2)} \tab \code{\link{strprint}} \cr
#'   \code{print(n = 2)} \tab \code{\link[base]{print}} \cr
#'   \code{summary(full = T)} \tab \code{\link{summary.Distribution}} \cr
#'   }
#'
#' @section Active Bindings:
#'  \tabular{ll}{
#'   \strong{Active Binding} \tab \strong{Link} \cr
#'   \code{isPdf} \tab \code{\link{isPdf}} \cr
#'   \code{isCdf} \tab \code{\link{isCdf}} \cr
#'   \code{isQuantile} \tab \code{\link{isQuantile}} \cr
#'   \code{isRand} \tab \code{\link{isRand}} \cr
#'   }
#'
#'
#' @seealso See \CRANpkg{set6} for details on Sets and Intervals. See \code{\link{ParameterSet}} for
#' parameter details. See \code{\link{decorate}} for Decorator details.
#'
#' @return Returns R6 object of class Distribution.
#'
#' @export
NULL
#-------------------------------------------------------------
# Distribution Definition
#-------------------------------------------------------------
Distribution <- R6Class("Distribution", lock_objects = FALSE)


#-------------------------------------------------------------
# Public Methods - Constructor
#-------------------------------------------------------------
Distribution$set("public","initialize",function(name = NULL, short_name = NULL,
                      type = NULL, support = NULL,
                      symmetric = FALSE,
                      pdf = NULL, cdf = NULL, quantile = NULL, rand = NULL,
                      parameters = NULL, decorators = NULL, valueSupport = NULL, variateForm = NULL,
                      description=NULL, suppressMoments = FALSE, .suppressChecks = FALSE
                      ){

  if(.suppressChecks | inherits(self, "DistributionWrapper")){

    if(!is.null(parameters)) parameters = parameters$clone(deep = TRUE)
    if(!is.null(pdf)) formals(pdf) = c(formals(pdf),list(self=self),alist(...=))
    if(!is.null(cdf)) formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
    if(!is.null(quantile)) formals(quantile) = c(formals(quantile),list(self=self),alist(...=))
    if(!is.null(rand)) formals(rand) = c(formals(rand),list(self=self),alist(...=))

  } else if(getR6Class(self) == "Distribution") {

    if(is.null(pdf) & is.null(cdf))
      stop("One of pdf or cdf must be provided.")

    #------------
    # Name Checks
    #------------
    if(is.null(name) & is.null(short_name))
      checkmate::assert("One of 'name' or 'short_name' must be provided.")
    if(is.null(short_name)) short_name = gsub(" ","",name,fixed = T)
    if(is.null(name)) name = short_name
    checkmate::assertCharacter(c(name, short_name),
                               .var.name = "'name' and 'short_name' must be of class 'character'.")
    checkmate::assert(length(strsplit(short_name,split=" ")[[1]])==1,
                      .var.name = "'short_name' must be one word only.")

    #------------------------
    # Type and Support Checks
    #------------------------
    if(is.null(type)){
      rm = c("...","self")
      if(!is.null(pdf)){
        lng = length(formals(pdf)[!(names(formals(pdf)) %in% rm)])
        type <- setpower(Reals$new(), lng)
      } else{
        lng = length(formals(cdf)[!(names(formals(cdf)) %in% rm)])
        type <- setpower(Reals$new(), lng)
      }
    }
    if(is.null(support)) support <- type
    assertSet(type)
    assertSet(support)

    #--------------------
    # valueSupport Checks
    #--------------------
    if(!is.null(valueSupport)){
      valueSupport = match.arg(valueSupport, c("continuous", "discrete", "mixture"))
    } else if(support$properties$countability == "uncountable") {
      valueSupport = "continuous"
     } else {
      valueSupport = "discrete"
     }

    #-------------------
    # variateForm Checks
    #-------------------
    if(!is.null(variateForm)){
      variateForm = match.arg(variateForm, c("univariate", "multivariate", "matrixvariate"))
    } else if(getR6Class(type) %in% c("ProductSet","ExponentSet")) {
      variateForm = "multivariate"
    } else {
      variateForm = "univariate"
    }


    #-------------------
    # pdf and cdf Checks
    #-------------------
    if(!is.null(pdf)){
      if(!is.null(formals(pdf)$self))
        formals(pdf)$self = self
      else
        formals(pdf) = c(formals(pdf),list(self=self),alist(...=))
    }

    if(!is.null(cdf)){
      if(!is.null(formals(cdf)$self))
        formals(cdf)$self = self
      else
        formals(cdf) = c(formals(cdf),list(self=self),alist(...=))
    }

    if(!is.null(pdf) & !is.null(cdf)){
      checkmate::assert(length(formals(pdf)) == length(formals(cdf)),
                        .var.name = "'pdf' and 'cdf' must take the same arguments.")
      checkmate::assert(all(names(formals(pdf)) == names(formals(cdf))),
                        .var.name = "'pdf' and 'cdf' must take the same arguments.")
    }

    #-------------------------
    # quantile and rand Checks
    #-------------------------
    if(!is.null(quantile)){
      if(!is.null(formals(quantile)$self))
        formals(quantile)$self = self
      else
        formals(quantile) = c(formals(quantile),list(self=self),alist(...=))
    }

    if(!is.null(rand)){
      if(!is.null(formals(rand)$self))
        formals(rand)$self = self
      else
        formals(rand) = c(formals(rand),list(self=self),alist(...=))
    }

    #-------------------------
    # Parameter Checks
    #-------------------------
    if(!is.null(parameters)){
      checkmate::assertClass(parameters,"ParameterSet")
      parameters <- parameters$clone(deep = TRUE)$update()
    }
  }

  if(!is.null(parameters))
    private$.parameters <- parameters

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

  if(!is.null(name)) self$name <- name
  if(!is.null(short_name)) self$short_name <- short_name

  private$.properties$support <- support
  private$.traits$type <- type
  private$.traits$valueSupport <- valueSupport
  private$.traits$variateForm <- variateForm

  if(!is.null(description)) self$description <- description

  symm = ifelse(symmetric,"symmetric","asymmetric")
  private$.properties$symmetry <- symm

  if(!is.null(decorators))
    suppressMessages(decorate(self, decorators))

  if(!suppressMoments){
    # Update skewness and kurtosis
    kur = try(self$kurtosis(excess = TRUE), silent = TRUE)
    skew = try(self$skewness(excess = TRUE), silent = TRUE)
    private$.properties$kurtosis <- ifnerror(kur, exkurtosisType(kur), "NULL")
    private$.properties$skewness <- ifnerror(skew, skewnessType(skew), "NULL")
  }

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
#' @param n Number of parameters to display before & after ellipsis
#' @usage strprint(object, n = 2)
#'
#' @return String representation of the distribution.
#'
#' @examples
#' Triangular$new()$strprint()
#' Triangular$new()$strprint(1)
#'
#' @export
Distribution$set("public","strprint",function(n = 2){
  if(length(private$.parameters)!=0){
    settable = as.data.table(self$parameters())$settable
    id = as.data.table(self$parameters())[settable, "id"][[1]]
    value = as.data.table(self$parameters())[settable, "value"][[1]]
    lng <- length(id)
    if(lng >(2*n))
      string = paste0(self$short_name, "(", paste(id[1:n], value[1:n], sep = " = ", collapse = ", "),
                      ",...,", paste(id[(lng-n+1):lng], value[(lng-n+1):lng], sep = " = ", collapse = ", "),")")
    else
      string = paste0(self$short_name, "(", paste(id, value, sep = " = ", collapse = ", "), ")")

  } else {
    string = paste0(self$short_name)
  }
  return(string)
})
Distribution$set("public","print",function(n = 2, ...){
  cat(self$strprint(n = n),"\n")
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
#' @return Printed summary of the distribution.
#'
#' @export
summary.Distribution <- function(object, full = TRUE,...) {}
Distribution$set("public","summary",function(full = TRUE,...){

  if(full){
    if(length(private$.parameters)!=0){

      cat(self$description,"Parameterised with:\n")
      settable = as.data.table(self$parameters())$settable
      cat(" ", paste(as.data.table(self$parameters())[settable, "id"],
                     as.data.table(self$parameters())[settable, "value"],
                      sep = " = ", collapse = ", "))
    } else
      cat(self$description)


    a_exp = suppressMessages(try(self$mean(), silent = T))
    a_var = suppressMessages(try(self$variance(), silent = T))
    a_skew = suppressMessages(try(self$skewness(), silent = T))
    a_kurt = suppressMessages(try(self$kurtosis(), silent = T))

    if(!inherits(a_exp,"try-error") | !inherits(a_var,"try-error") |
       !inherits(a_skew,"try-error") | !inherits(a_kurt,"try-error"))
      cat("\n\n ", "Quick Statistics","\n")

    if(!inherits(a_exp,"try-error")){
      cat("\tMean:")
      if(length(a_exp) > 1)
        cat("\t\t", paste0(a_exp, collapse = ", "),"\n", sep = "")
      else
        cat("\t\t", a_exp,"\n", sep = "")
    }
    if(!inherits(a_var,"try-error")){
      cat("\tVariance:")
      if(length(a_var) > 1)
        cat("\t", paste0(a_var, collapse = ", "),"\n", sep = "")
      else
        cat("\t", a_var,"\n", sep = "")
    }
    if(!inherits(a_skew,"try-error")){
      cat("\tSkewness:")
      # if(length(a_skew) > 1)
      #   cat("\t", paste0(a_skew, collapse = ", "),"\n", sep = "")
      # else
        cat("\t", a_skew,"\n", sep = "")
    }
    if(!inherits(a_kurt,"try-error")){
      cat("\tEx. Kurtosis:")
      # if(length(a_kurt) > 1)
      #   cat("\t", paste0(a_kurt, collapse = ", "),"\n", sep = "")
      # else
        cat("\t", a_kurt,"\n", sep = "")
    }
    cat("\n")

    cat(" Support:",self$properties$support$strprint(), "\tScientific Type:",self$traits$type$strprint(),"\n")
    cat("\n Traits:\t",self$valueSupport,"; ",self$variateForm, sep="")
    cat("\n Properties:\t", self$symmetry,sep="")
    if(!inherits(a_kurt,"try-error")) cat(";",self$kurtosisType)
    if(!inherits(a_skew,"try-error")) cat(";",self$skewnessType)

    if(length(self$decorators)!=0)
      cat("\n\n Decorated with: ", paste0(self$decorators,collapse=", "))

  } else {
    if(length(private$.parameters)!=0)
      cat(self$strprint())
    else
      cat(self$name)
    cat("\nScientific Type:",self$traits$type$strprint(),"\t See $traits for more")
    cat("\nSupport:",self$properties$support$strprint(),"\t See $properties for more")
  }
  cat("\n")
  invisible(self)
})

#-------------------------------------------------------------
# Public Methods - Parameters
#-------------------------------------------------------------
# Documented in ParameterSet.R
Distribution$set("public","parameters",function(id = NULL){
  if(length(private$.parameters)==0)
    return(NULL)
  else
    return(private$.parameters$parameters(id))
})
# Documented in ParameterSet.R
Distribution$set("public","getParameterValue",function(id, error = "warn"){
  if(length(private$.parameters)==0)
    return(NULL)
  else
    return(private$.parameters$getParameterValue(id, error))
})
# Documented in ParameterSet.R
Distribution$set("public","setParameterValue",function(..., lst = NULL, error = "warn"){
  if(is.null(lst))
    lst <- list(...)
  if(length(private$.parameters)==0)
    return(NULL)
  else{
    if(length(lst)!=0){

      self$parameters()$setParameterValue(lst = lst, error = error)

      # Update skewness and kurtosis
      x = suppressMessages(try(self$kurtosis(excess = TRUE), silent = TRUE))
      if(class(x) == "try-error")
        private$.properties$kurtosis <- NULL
      else
        private$.properties$kurtosis <- exkurtosisType(x)

      x = suppressMessages(try(self$skewness(), silent = TRUE))
      if(class(x) == "try-error")
        private$.properties$skewness <- NULL
      else
        private$.properties$skewness <- skewType(x)

      #private$.setWorkingSupport()
      invisible(self)
    }
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
#' @return Probability density function evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a data.table.
#'
#' @seealso \code{\link{cdf}}, \code{\link{quantile}}, \code{\link{rand}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @export
NULL
Distribution$set("public","pdf",function(..., log = FALSE, simplify = TRUE, data = NULL){

  if (is.null(private$.pdf)){
    return(NULL)
  }

  data = pdq_point_assert(..., self = self, data = data)
  assert(self$liesInType(as.numeric(data), all = TRUE, bound = FALSE),
         .var.name = "Do all points lie in Distribution domain?")

  if(log){
    if(private$.log){
      pdqr = private$.pdf(data, log = log)
    } else {
      if(!("CoreStatistics" %in% self$decorators)){
        stop("No analytical method for log available. Use CoreStatistics decorator to numerically estimate this.")
      } else {
        pdqr = log(private$.pdf(data))
      }
    }
  } else {
    pdqr = private$.pdf(data)
  }

  pdqr_returner(pdqr, simplify)
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
#' @return Cumulative distribution function evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a data.table.
#'
#' @export
NULL
Distribution$set("public","cdf",function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL){

  if (is.null(private$.cdf)) {
    return(NULL)
  }

  data = pdq_point_assert(..., self = self, data = data)
  assert(self$liesInType(as.numeric(data), all = TRUE, bound = FALSE),
         .var.name = "Do all points lie in Distribution domain?")

  if(log.p | !lower.tail){
    if(private$.log){
      pdqr = private$.cdf(data, log.p = log.p, lower.tail = lower.tail)
    } else {
      if(!("CoreStatistics" %in% self$decorators)){
        stop("No analytical method for log.p or lower.tail available. Use CoreStatistics decorator to numerically estimate this.")
      } else {
        pdqr = private$.cdf(data)
        if(!lower.tail) pdqr = 1 - cdf
        if(log.p) pdqr = log(cdf)
      }
    }
  } else {
    pdqr = private$.cdf(data)
  }

  pdqr_returner(pdqr, simplify)
})
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
#' @param log.p logical; if TRUE then \eqn{q_X(exp(p))} is returned.
#' @param simplify if TRUE (default) returns results in simplest form (vector or data.table) otherwise as data.table.
#'
#' @details
#'  The quantile function, \eqn{q_X}, is the inverse cdf, i.e.
#'  \deqn{q_X(p) = F^{-1}_X(p) = \inf\{x \in R: F_X(x) \ge p\}}{q_X(p) = F^(-1)_X(p) = inf{x \epsilon R: F_X(x) \ge p}}
#'
#'  If \code{lower.tail} is FALSE then \eqn{q_X(1-p)} is returned.
#'
#' If available a quantile will be returned without warning using an analytic expression. Otherwise,
#' if the distribution has not been decorated with \code{FunctionImputation}, \code{NULL} is returned.
#' To impute the quantile, use \code{decorate(distribution, FunctionImputation)}, this will provide a numeric
#' calculation for the quantile with warning.
#'
#' Additional named arguments can be passed, which are required for composite distributions such as
#' \code{\link{ProductDistribution}} and \code{\link{VectorDistribution}}.
#'
#' @seealso \code{\link{pdf}}, \code{\link{cdf}}, \code{\link{rand}} for other statistical functions.
#' \code{\link{FunctionImputation}}, \code{\link{decorate}} for imputing missing functions.
#'
#' @return Inverse cumulative distribution function evaluated at given points as either a numeric if \code{simplify} is TRUE
#' or as a data.table.
#'
#' @export
quantile.Distribution <- function(x, ..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL) {}
Distribution$set("public","quantile",function(..., lower.tail = TRUE, log.p = FALSE, simplify = TRUE, data = NULL){

  if (is.null(private$.quantile)) {
    return(NULL)
  }

  data = pdq_point_assert(..., self = self, data = data)
  assert(Interval$new(0,1)$contains(as.numeric(data), all = TRUE),
         .var.name = "Do all quantiles lie in [0,1]?")

  if(log.p | !lower.tail){
    if(private$.log){
      pdqr = private$.quantile(data, log.p = log.p, lower.tail = lower.tail)
    } else {
      if(!("CoreStatistics" %in% self$decorators)){
        stop("No analytical method for log.p or lower.tail available. Use CoreStatistics decorator to numerically estimate this.")
      } else {
        if(log.p) data = exp(data)
        if(!lower.tail) data = 1 - data
        pdqr = private$.quantile(data)
      }
    }
  } else {
    pdqr = private$.quantile(data)
  }

  pdqr_returner(pdqr, simplify)
})
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
#' @return Simulated draws from the distribution as either a numeric if \code{simplify} is TRUE
#' or as a data.table.
#'
#' @export
NULL
Distribution$set("public","rand",function(n, simplify = TRUE){

  if (is.null(private$.rand)) {
    return(NULL)
  }

  if(length(n) > 1) n = length(n)

  pdqr = private$.rand(n)

  pdqr_returner(pdqr, simplify)
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
#' @return Reciprocal of variance as a numeric.
#'
#' @seealso \code{\link{variance}}
#'
#' @export
NULL
Distribution$set("public","prec",function(){
  return(1/self$variance())
})

#' @name stdev
#' @title Standard Deviation of a Distribution
#' @description Standard deviation of a distribution assuming variance is provided.
#'
#' @usage stdev(object)
#' @section R6 Usage: $stdev()
#' @param object Distribution.
#' @details The standard deviation is analytically computed as the square root of the variance.
#' If the variance is not found in the distribution (analytically or numerically), returns error.
#' @return Square-root of variance as a numeric.
#'
#' @seealso \code{\link{variance}}
#'
#' @export
NULL
Distribution$set("public","stdev",function(){
  return(sqrt(self$variance()))
})

#' @title Median of a Distribution
#' @description Median of a distribution assuming quantile is provided.
#'
#' @importFrom stats median
#' @method median Distribution
#' @section R6 Usage: $median()
#' @param x Distribution.
#' @param na.rm ignored, added for consistency with S3 generic.
#' @param ... ignored, added for consistency with S3 generic.
#' @details The median is computed as the quantile function evaluated at 0.5.
#' If the quantile is not found in the distribution (analytically or numerically), returns error.
#' @return Quantile function evaluated at 0.5 as a numeric.
#'
#' @seealso \code{\link{quantile.Distribution}}
#'
#' @export
median.Distribution <- function(x, na.rm = NULL, ...) {}
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
#' @return Interquartile range of distribution as a numeric.
#'
#' @export
NULL
Distribution$set("public", "iqr", function() {
  return(self$quantile(0.75) - self$quantile(0.25))
})

#' @title Distribution Correlation
#' @name correlation
#' @description Correlation of a distribution.
#'
#' @usage correlation(object)
#' @section R6 Usage: $correlation()
#'
#' @param object Distribution.
#'
#' @details In terms of covariance, the correlation of a distribution is defined by the equation,
#' \deqn{\rho_{XY} = \sigma_{XY}/\sigma_X\sigma_Y}
#' where \eqn{\sigma_{XY}} is the covariance of X and Y and \eqn{\sigma_X, \sigma_Y} and the respective
#' standard deviations of X and Y.
#'
#' If the distribution is univariate then returns \eqn{1}.
#'
#' Calculates correlation analytically from variance. If an analytic expression for variance isn't available,
#' returns error. To impute a numeric expression, use the \code{\link{CoreStatistics}} decorator.
#'
#' @return Either '1' if distribution is univariate or the correlation as a numeric or matrix.
#'
#' @export
NULL
Distribution$set("public","correlation",function(){
  if(testUnivariate(self))
    return(1)
  else
    return(self$variance() / (sqrt(diag(self$variance()) %*% t(diag(self$variance())))))
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
#' @return Either a vector of logicals if \code{all} is FALSE otherwise returns TRUE if every element
#' lies in the distribution support or FALSE otherwise.
#'
#' @seealso \code{\link{liesInType}}
#'
#' @export
NULL
Distribution$set("public","liesInSupport",function(x, all = TRUE, bound = FALSE){
  return(self$properties$support$contains(x, all, bound))
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
#' @return Either a vector of logicals if \code{all} is FALSE otherwise returns TRUE if every element
#' lies in the distribution type or FALSE otherwise.
#'
#' @seealso \code{\link{liesInSupport}}
#'
#' @export
NULL
Distribution$set("public","liesInType",function(x, all = TRUE, bound = FALSE){
  return(self$traits$type$contains(x, all, bound))
})

#-------------------------------------------------------------
# Distribution Active Bindings
#-------------------------------------------------------------
#' @name decorators
#' @title Decorators Accessor
#' @usage decorators(object)
#' @section R6 Usage: $decorators
#' @param object Distribution.
#' @description Returns the decorators added to a distribution.
#' @seealso \code{\link{decorate}}
#' @return Character vector of decorators.
#' @export
NULL
Distribution$set("active","decorators", function(){
  return(private$.decorators)
})

#' @name traits
#' @title Traits Accessor
#' @usage traits(object)
#' @section R6 Usage: $traits
#' @param object Distribution.
#' @description Returns the traits of the distribution.
#' @return List of traits.
#' @export
NULL
Distribution$set("active","traits",function(){
  return(private$.traits)
})

#' @name valueSupport
#' @title Value Support Accessor - Deprecated
#' @usage valueSupport(object)
#' @param object Distribution.
#' @description Deprecated. Use $traits$valueSupport
#' @return One of "discrete"/"continuous"/"mixture".
#' @export
NULL
Distribution$set("active","valueSupport",function(){
  return("Deprecated. Use $properties$valueSupport instead.")
})

#' @name variateForm
#' @title Variate Form Accessor - Deprecated
#' @usage variateForm(object)
#' @param object Distribution.
#' @description Deprecated. Use $traits$variateForm
#' @return One of "univariate"/"multivariate"/"matrixvariate".
#' @export
NULL
Distribution$set("active","variateForm",function(){
  return("Deprecated. Use $traits$variateForm instead.")
})

#' @name type
#' @title Type Accessor - Deprecated
#' @usage type(object)
#' @section R6 Usage: $type
#' @param object Distribution.
#' @description Deprecated. Use $traits$type
#' @return An R6 object of class [set6::Set].
#' @seealso [set6::Set]
#' @export
NULL
Distribution$set("active","type",function(){
  return("Deprecated. Use $traits$type instead.")
})

#' @name properties
#' @title Properties Accessor
#' @usage properties(object)
#' @section R6 Usage: $properties
#' @param object Distribution.
#' @description Returns the properties of the distribution.
#' @return List of distribution properties.
#' @export
NULL
Distribution$set("active","properties",function(x){
  private$.properties
})

#' @name support
#' @title Support Accessor - Deprecated
#' @usage support(object)
#' @section R6 Usage: $support
#' @param object Distribution.
#' @description Deprecated. Use $properties$support
#' @details The support of a probability distribution is defined as the interval where the pmf/pdf is
#' greater than zero,
#' \deqn{Supp(X) = \{x \ \in R: \ f_X(x) \ > \ 0\}}{Supp(X) = {x \epsilon R: f_X(x) > 0}}
#' where \eqn{f_X} is the pmf if distribution \eqn{X} is discrete, otherwise the pdf.
#' @return An R6 object of class [set6::Set].
#' @seealso [set6::Set] and \code{\link{properties}}
#' @export
NULL
Distribution$set("active","support",function(){
  return("Deprecated. Use $properties$support instead.")
})

#' @name symmetry
#' @title Symmetry Accessor - Deprecated
#' @usage symmetry(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$symmetry.
#' @return One of "symmetric" or "asymmetric".
#' @seealso \code{\link{properties}}
#' @export
NULL
Distribution$set("active","symmetry",function(){
  return("Deprecated. Use $properties$symmetry instead.")
})

#' @name sup
#' @title Supremum Accessor
#' @usage sup(object)
#' @section R6 Usage: $sup
#' @param object Distribution.
#' @description Returns the distribution supremum as the supremum of the support.
#' @return Supremum as a numeric.
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{dmin}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("active","sup",function(){
  return(self$properties$support$upper)
})

#' @name inf
#' @title Infimum Accessor
#' @usage inf(object)
#' @section R6 Usage: $inf
#' @param object Distribution.
#' @description Returns the distribution infimum as the infimum of the support.
#' @return Infimum as a numeric.
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{dmin}}, \code{\link{sup}}
#' @export
NULL
Distribution$set("active","inf",function(){
  return(self$properties$support$lower)
})

#' @name dmax
#' @title Distribution Maximum Accessor
#' @usage dmax(object)
#' @section R6 Usage: $dmax
#' @param object Distribution.
#' @description Returns the distribution maximum as the maximum of the support. If the support is not
#' bounded above then maximum is given by
#' \deqn{maximum = supremum - 1.1e-15}
#' @return Maximum as a numeric.
#' @seealso \code{\link{support}}, \code{\link{dmin}}, \code{\link{sup}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("active","dmax",function(){
  return(self$properties$support$max)
})

#' @name dmin
#' @title Distribution Minimum Accessor
#' @usage dmin(object)
#' @section R6 Usage: $dmin
#' @param object Distribution.
#' @description Returns the distribution minimum as the minimum of the support. If the support is not
#' bounded below then minimum is given by
#' \deqn{minimum = infimum + 1.1e-15}
#' @return Minimum as a numeric.
#' @seealso \code{\link{support}}, \code{\link{dmax}}, \code{\link{sup}}, \code{\link{inf}}
#' @export
NULL
Distribution$set("active","dmin",function(){
  return(self$properties$support$min)
})

#' @name kurtosisType
#' @title Type of Kurtosis Accessor - Deprecated
#' @usage kurtosisType(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$kurtosis.
#' @return If the distribution kurtosis is present in properties, returns one of "platykurtic"/"mesokurtic"/"leptokurtic",
#' otherwise returns NULL.
#' @seealso \code{\link{kurtosis}}, \code{\link{properties}} and \code{\link{skewnessType}}
#' @export
NULL
Distribution$set("active", "kurtosisType", function() {
  return("Deprecated. Use $properties$kurtosis instead.")
})

#' @name skewnessType
#' @title Type of Skewness Accessor - Deprecated
#' @usage skewnessType(object)
#' @param object Distribution.
#' @description Deprecated. Use $properties$skewness.
#' @return If the distribution skewness is present in properties, returns one of "negative skew", "no skew",
#' "positive skew", otherwise returns NULL.
#' @seealso \code{\link{skewness}}, \code{\link{properties}} and \code{\link{kurtosisType}}
#' @export
NULL
Distribution$set("active", "skewnessType", function() {
  return("Deprecated. Use $properties$skewness instead.")
})

#' @name isPdf
#' @rdname isPdf
#' @title Test the Distribution Pdf Exist?
#' @description Returns whether or not the distribution has a defined expression for the pdf.
#' @section R6 Usage: $isPdf
#' @return Returns \code{TRUE} if an expression for the pdf is defined for the distribution, \code{FALSE}
#' otherwise.
#' @seealso \code{\link{isCdf}}, \code{\link{isQuantile}}, \code{\link{isRand}}
NULL
Distribution$set("active","isPdf",function() return(private$.isPdf))
#' @name isCdf
#' @rdname isCdf
#' @title Test the Distribution Cdf Exist?
#' @description Returns whether or not the distribution has a defined expression for the Cdf.
#' @section R6 Usage: $isCdf
#' @return Returns \code{TRUE} if an expression for the Cdf is defined for the distribution, \code{FALSE}
#' otherwise.
#' @seealso \code{\link{isPdf}}, \code{\link{isQuantile}}, \code{\link{isRand}}
NULL
Distribution$set("active","isCdf",function() return(private$.isCdf))
#' @name isQuantile
#' @rdname isQuantile
#' @title Test the Distribution Quantile Exist?
#' @description Returns whether or not the distribution has a defined expression for the Quantile.
#' @section R6 Usage: $isQuantile
#' @return Returns \code{TRUE} if an expression for the Quantile is defined for the distribution, \code{FALSE}
#' otherwise.
#' @seealso \code{\link{isPdf}}, \code{\link{isCdf}}, \code{\link{isRand}}
NULL
Distribution$set("active","isQuantile",function() return(private$.isQuantile))
#' @name isRand
#' @rdname isRand
#' @title Test the Distribution Rand Exist?
#' @description Returns whether or not the distribution has a defined expression for the Rand.
#' @section R6 Usage: $isRand
#' @return Returns \code{TRUE} if an expression for the Rand is defined for the distribution, \code{FALSE}
#' otherwise.
#' @seealso \code{\link{isPdf}}, \code{\link{isCdf}}, \code{\link{isQuantile}}
NULL
Distribution$set("active","isRand",function() return(private$.isRand))

#-------------------------------------------------------------
# Distribution Public Variables
#-------------------------------------------------------------
Distribution$set("public","name",character(0))
Distribution$set("public","short_name",character(0))
Distribution$set("public","description",NULL)
# Distribution$set("private",".pdf", function() {})
# Distribution$set("private",".cdf", function() {})
# Distribution$set("private",".quantile", function() {})
# Distribution$set("private",".rand", function() {})
Distribution$set("private",".parameters",NULL)
Distribution$set("private",".workingSupport",NULL)
Distribution$set("private",".decorators", NULL)
Distribution$set("private",".properties", list())
Distribution$set("private",".traits",NULL)
# Distribution$set("private",".isPdf", FALSE)
# Distribution$set("private",".isCdf", FALSE)
# Distribution$set("private",".isQuantile", FALSE)
# Distribution$set("private",".isRand", FALSE)
Distribution$set("private", ".log", FALSE)

#-------------------------------------------------------------
# Distribution Private Methods
#-------------------------------------------------------------
Distribution$set("private",".setWorkingSupport",function(){
  suppressMessages({
    rands = self$rand(1000)

    if(self$sup != Inf)
      newsup = self$sup
    else{
      newsup = ceiling(max(rands))
      while(self$pdf(newsup) > .Machine$double.eps) newsup = newsup + 1
      newsup = ceiling(newsup - 1)
    }

    inf = self$inf
    if(inf != -Inf)
      newinf = inf
    else{
      newinf = floor(min(rands))
      while(self$pdf(newinf) > .Machine$double.eps) newinf = newinf - 1
      newinf = floor(newinf + 1)
    }

    private$.workingSupport <- list(inf = newinf, sup = newsup)
    invisible(self)
  })
})
Distribution$set("private",".getWorkingSupport",function(){
  return(private$.workingSupport)
})
Distribution$set("private",".updateDecorators", function(decs){
  private$.decorators <- decs
})
