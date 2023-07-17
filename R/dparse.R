#' @title Parse Distributions Represented as Strings
#' @description Parse a custom string that represents an R6 distribution
#' @details Transform a custom (user) input to a R6 object.
#'
#' This function is specially useful when you expect a user input which should
#' result in specific distribution. The distribution name must be the
#' ShortName, ClassName or Alias listed in the package, which can be found with
#' [listDistributions()].
#'
#' @param toparse `(character(1))`\cr
#' String to parse, which should be in the format Distribution(\[params\]), see examples.
#'
#' @return Returns an R6 [Distribution]
#'
#' @examples
#' dparse("N()")
#' dparse("norm(0, sd = 2)")
#' # lower and upper case work
#' dparse("n(sd = 1, mean = 4)")
#' dparse("T(df = 4)")
#' # be careful to escape strings properly
#' dparse("C(list('A', 'B'), c(0.5, 0.5))")
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
    stop(sprintf("Call '%s' could not be evaluated as distribution '%s' not found.",
                 toparse, dist))
  }
  # Call and evaluate
  call <- sprintf("distr6::%s$new(%s)", class, params)
eval(parse(text = call))
}
