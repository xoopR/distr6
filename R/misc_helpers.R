#' @title Test if Message Produced
#' @description Tests if a given expression produces a message.
#' @param expr expression to test.
#' @return TRUE if a message is prodcued, FALSE otherwise.
#' @examples
#' msgFun <- function() message("Test Function")
#' noMsgFun <- function() message("Test Function")
#' testMessage(msgFun) # TRUE
#' testMessage(noMsgFun) # FALSE
#' @export
testMessage <- function(expr){
  if(inherits(tryCatch(expr, message = function(m) m), "message"))
    return(TRUE)
  else
    return(FALSE)
}

#' @title Helper Function for Validation Errors
#' @description Matches error type to stop or warning and produces the respective error and
#' returns NULL.
#' @param error one of 'warn' or 'stop'. Taken as warning if not 'stop'.
#' @param error.msg string to parse as error message.
#' @return error if 'error' == 'stop'; NULL and warning otherwise.
#' @examples
#'
#' \dontrun{
#' if(2 > 1) stopwarn("warn", "This number is too big")
#' if(2 < 3) stopwarn("stop", "This number is too small")
#' }
#'
#' @export
stopwarn <- function(error = "warn", error.msg){
  checkmate::assert(error == "warn", error == "stop",
                    .var.name = "'error' should be one of 'warn' or 'stop'.")
  if(error == "stop")
    stop(error.msg)
  else{
    warning(error.msg, call. = F)
    return(NULL)
  }
}
