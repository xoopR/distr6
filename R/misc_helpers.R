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
