tuple <- function(...) {
 ll <- list(...)
 len <- length(ll)
 if(len == 0) {
   stop("There should be non-zero arguments")
 }
 if(len == 1) {

   if(!is.list(...)) {
     stop("Input is not a list")
   } else {
     return(new ("tuple", ...))
   }
 } else {
    nms <- names(ll)
    if(any(unlist(lapply(names(ll), nchar)) == 0)) {
      stop("The tuple must be a named list")
    } else {
      return(new ("tuple", ll))
    }
    
 }
 
 return(ll)
 #new ("tuple", list(...))
}

setClass("tuple", contains = "list")

