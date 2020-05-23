#' @details Decorator objects add functionality to the given [Distribution] object by copying methods
#' in the decorator environment to the chosen [Distribution] environment.
#'
#' All methods implemented in decorators try to exploit analytical results where possible, otherwise
#' numerical results are used with a message.
#'
#' @family decorators
