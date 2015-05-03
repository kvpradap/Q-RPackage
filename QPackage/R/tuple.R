#' Tuple constructor
#' Tuple contructor that can take in a R named list and return Tuple object
#' 
#' @examples
#' 
#' \dontrun{
#' t <- tuple("title"="This is title",  "brand" = "Product brand")
#' }
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
s
print.tuple <- function(t) {
  data.frame(t)
}

#' An S4 class to represent a tuple.
#' 
setClass("tuple", contains = "list")

