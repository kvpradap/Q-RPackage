#' Fix a qtable
#' 
#' \code{fix} internal invokes \code{\link{edit}} on qtable object and
#' stores back the updated version of qtable object.
#' 
#' @param x qtable object
#' @param ... Relevant paramaters to \code{\link{edit}} function.
#' 
#' @examples 
#' 
#' \donotrun{
#' fix(qtable_obj)
#' }
fix <- function(x,...) {
  if (isS4(x) == TRUE) {
    if(class(x)[[1]] == "qtable") {
      key <- x@key
      subx <- substitute(x)
      if (is.name(subx)) 
        subx <- deparse(subx)
      if (!is.character(subx) || length(subx) != 1L) 
        stop("'fix' requires a name")
      parent <- parent.frame()
      if (exists(subx, envir = parent, inherits = TRUE)) 
        x <- edit(get(subx, envir = parent), title = subx, ...)
      else {
        x <- edit(function() {
        }, title = subx, ...)
        environment(x) <- .GlobalEnv
      }
      x <- qtable(x)
      set_id(x, key)     
      assign(subx, x, envir = .GlobalEnv)
    
    }
  } else {
    utils::fix(x)
  }
  
  
  
  
  
}