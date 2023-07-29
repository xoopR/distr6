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
#' dparse("cHiSq(df = 3)")
#' # be careful to escape strings properly
#' dparse("C(list('A', 'B'), c(0.5, 0.5))")
#' dparse("Cat(elements = c('A', 'B'), probs = c(0.5, 0.5))")
#' @export
dparse <- function(toparse) {
  # Check that the input is in a valid format
  if (!grepl("^[a-zA-Z0-9]+\\(.*\\)$", toparse)) {
    stop(sprintf("Call '%s' does not have a valid format. See documentation.",
                 toparse))
  }
  # Extract distribution and parameters
  dist <- gsub("(^.*?)\\(.*", "\\1", toparse)
  params <- tolower(gsub(".*?\\((.*)\\)$", "\\1", toparse))
  # Join all identifiers and locate the correct class
  d6s <- listDistributions()
  d6s$ids <- paste(tolower(d6s$ShortName), tolower(d6s$ClassName), tolower(d6s$Alias),
                   sep = ", ")
  dist_word <- paste0("\\b", tolower(dist), "\\b")
  classrow <- which(grepl(dist_word, d6s$ids))
  # Distribution not found
  if (length(classrow) != 1) {
    stop(sprintf("Call '%s' could not be evaluated as distribution '%s' not found.",
                 toparse, dist))
  }
  class <- d6s[[classrow, "ClassName"]]
  # Call and evaluate
  call <- sprintf("distr6::%s$new(%s)", class, params)
  eval(parse(text = call))
}
