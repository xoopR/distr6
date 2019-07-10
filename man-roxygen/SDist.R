#' @title <%=DistName%> Distribution Class
#'
#' @description Mathematical and statistical functions for the <%=DistName%> distribution, which
#' is commonly used <%=uses%>.
#'
#' @details The <%=DistName%> distribution parameterised with <%=params%> is defined by the <%=pdfpmf%>,
#' <%=pdfpmfeq%> for <%=paramsupport%>.
#'
#' The distribution is supported on <%=distsupport%>.
#'
#' <%= if(exists("omittedVars")) omittedVars %>
#' <%= if(exists("omittedVars")) ifelse(length(unlist(strsplit(omittedVars,split=" ",fixed = T)))==1, "is", "are") %>
#' <%= if(exists("omittedVars")) "omitted as no closed form analytic expression could be found, decorate with \\code{\\link{CoreStatistics}} for numerical results." %>
#' <%= if(exists("omittedDPQR")) omittedDPQR %>
#' <%= if(exists("omittedDPQR")) ifelse(length(unlist(strsplit(omittedDPQR,split=" ",fixed = T)))==1, "is", "are") %>
#' <%= if(exists("omittedDPQR")) "omitted as no closed form analytic expression could be found, decorate with \\code{\\link{FunctionImputation}} for a numerical imputation." %>
#'
#' <%= if(exists("aka")) "Also known as the "%><%= if(exists("aka")) aka%><%= if(exists("aka")) " distribution."%>
#' <%= if(exists("additionalDetails")) additionalDetails %>
#'
#' @section Constructor: <%=ClassName%>$new(<%=constructor%>, decorators = NULL, verbose = FALSE)
#' @section Constructor Arguments:
#' \tabular{lll}{
#'   \strong{Argument} \tab \strong{Type} \tab \strong{Details} \cr
#'   <%= if(exists("arg1")) arg1%>
#'   <%= if(exists("arg2")) arg2%>
#'   <%= if(exists("arg3")) arg3%>
#'   <%= if(exists("arg4")) arg4%>
#'   <%= if(exists("arg5")) arg5%>
#'   <%= if(exists("arg6")) arg6%>
#'   <%= if(exists("arg7")) arg7%>
#'   <%= if(exists("arg8")) arg8%>
#'   <%= if(exists("arg9")) arg9%>
#'   <%= if(exists("arg10")) arg10%>
#'   \code{decorators} \tab Decorator \tab decorators to add functionality. See details. \cr
#'   \code{verbose} \tab logical \tab if TRUE parameterisation messages produced.
#'   }
#'
#' @section Constructor Details: The <%=DistName%> distribution is parameterised with <%=constructorDets%>
#'
#' @inheritSection SDistribution Public Variables
#' @inheritSection SDistribution Public Methods
#'
#' @references
#' McLaughlin, M. P. (2001). A compendium of common probability distributions (pp. 2014-01).
#' Michael P. McLaughlin.
#'
#' <%= if(exists("additionalReferences")) additionalReferences %>
#'
#' @seealso
#' \code{\link{listDistributions}} for all available distributions. <%= if(exists("additionalSeeAlso")) additionalSeeAlso %> <%= if(exists("omittedVars")) "\\code{\\link{CoreStatistics}} for numerical results." %> <%= if(exists("omittedDPQR")) "\\code{\\link{FunctionImputation}} to numerically impute d/p/q/r." %>
#'

