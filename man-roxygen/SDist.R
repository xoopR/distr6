#' @title <%=DistName%> Distribution Class
#'
#' @description Mathematical and statistical functions for the <%=DistName%> distribution, which
#' is commonly used <%=uses%>.
#'
#' @details The <%=DistName%> distribution <%= if(exists("params")) "parameterised with " %> <%= if(exists("params")) params %> is defined by the <%=pdfpmf%>, <%=pdfpmfeq%> for <%=paramsupport%>.
#'
#' The distribution is supported on <%=distsupport%>.
#'
#' <%= if(exists("omittedDPQR")) omittedDPQR %>
#' <%= if(exists("omittedDPQR")) ifelse(length(unlist(strsplit(omittedDPQR,split=" ",fixed = T)))==1, "is", "are") %>
#' <%= if(exists("omittedDPQR")) "omitted as no closed form analytic expression could be found, decorate with \\code{\\link{FunctionImputation}} for a numerical imputation." %>
#'
#' <%= if(exists("aka")) "Also known as the "%><%= if(exists("aka")) aka%><%= if(exists("aka")) " distribution."%>
#'
#' @return Returns an R6 object inheriting from class SDistribution.
#'
#' @references
#' McLaughlin, M. P. (2001). A compendium of common probability distributions (pp. 2014-01).
#' Michael P. McLaughlin.

