#' @title String parses
#' @description Parse a custom string that represents an R6 distribution
#' @details Transform a custom (user) input to a R6 object.
#'
#' This function is specially useful when you expect a user input which should
#' result in specific distribution. The distribution name must be the
#' ShortName, ClassName or Alias listed in the package. You can list them
#' using the \link{listDistributions} function.
#'
#' Text call must be in the following format: Distribution(\[params\]), e.g.
#' T() or Normal(mean = 3).
#'
#' The parameters can be entered in any order, but they must be named. If no
#' parameter is given, the default parametrization provided in $new() call
#' is used.
#'
#' @param toparse character
#'
#' @return Returns an R6 distribution object
#'
#' @examples
#' dparse("N()")
#' dparse("T(df = 4)")
#' dparse("chisq(df = 3)")
#' @export
dparse <- function(toparse) {
  # Check correct format
  match <- grepl("^[a-zA-Z]+\\(.*\\)$", toparse)
  if (!match) stop(sprintf("Call '%s' is not valid. The required structure is: Distributino([params]), e.g. T() or Normal(mean = 3).",
                           toparse))
  # Extract distribution and parameters
  dist <- gsub("(^.*)\\(.*", "\\1", toparse)
  params <- tolower(gsub(".*\\((.*)\\)$", "\\1", toparse))
  # Locate correct class
  d6s <- listDistributions()[,c("ShortName", "ClassName", "Alias")]
  class <- d6s[tolower(ShortName) == tolower(dist) |
               tolower(ClassName) == tolower(dist) |
               tolower(Alias)     == tolower(dist),][["ClassName"]]
  # Distribution not found
  if (length(class) == 0) {
    stop(sprintf("Call '%s' could not be evaluated because distribution '%s' was not found.",
                 toparse, dist))
  }
  # Call and evaluate
  call <- sprintf("distr6::%s$new(%s)", class, params)
  r <- eval(parse(text = call))
  return(r)
}
