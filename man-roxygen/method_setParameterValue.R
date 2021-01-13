#' @param ... `ANY` \cr
#' Named arguments of parameters to set values for. See examples.
#' @param lst `(list(1))` \cr
#' Alternative argument for passing parameters. List names should be parameter names and list values
#' are the new values to set.
#' @param error `(character(1))` \cr
#' If `"warn"` then returns a warning on error, otherwise breaks if `"stop"`.
#' @param resolveConflicts `(logical(1))`\cr
#' If `FALSE` (default) throws error if conflicting parameterisations are provided, otherwise
#' automatically resolves them by removing all conflicting parameters.
