#' An S4 class to represent qtable
#' 
#' @slot key A list of attributes in qtable that uniquely identifies a row. 
#' The key values are unique and do not contain missing values.
#' 
qtable <- function(...) {
  q <- new ("qtable", data.frame(...))
  # attr(q, "key") <- NULL
}

setClassUnion("list.or.NULL", c("list", "NULL"))


setClass(
  "qtable", slots=c(key="list.or.NULL"),
  contains = "data.frame"
)

#' Check Id attribute
#' 
#' Checks whether a list of attributes contain unique values and does not have 
#' any missing values.
#' 
#' @param object qtable object.
#' @param key List of column (attribute) names from qtable object
#' @return A numeric value (0/1) indicating whether the list of attribute names can be
#'   set as key.
#'   
#' @examples
#' 
#'  \dontrun{
#'  status <- check_id(qtable_obj, list("id"))
#'  } 
setGeneric("check_id", function(object, key) standardGeneric("check_id"))


#' Set Id attribute
#' 
#' Sets a list of attributes as key to a qtable object.
#' 
#' @param object qtable object.
#' @param key List of column (attribute) names from qtable object
#' @return Returns a numeric status (0/1) indicating whether the function
#'   executed successfully.
#'   
#' @examples 
#' 
#' \dontrun{
#' status <- set_id(qtable_obj, list("id"))
#' }

setGeneric("set_id", function(object, key) standardGeneric("set_id"))



setMethod("check_id", representation(object="qtable", key="list"), function(object, key) {
  
  key <- unlist(key)
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    message('key can be only a character vector')
    return(1)
  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {
    message(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
    return(1)
  }
  
  # check unique values in key
  if(any(is.na(object[key]))) {
    message("key attribute contains missing values")
    return(1)
  }
  uniq_vals <- unique(object[key])
  if( nrow(uniq_vals) != nrow(object) ) {
    return(1)
  } else {
    return(0)
  }
})

setMethod("set_id", representation(object="qtable",  key="list"), function(object, key) {
  
  key <- unlist(key)
  
  if(length(key) == 0) {
    message('key is empty')
    return(1)
  }
  
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    message('key can be only a character vector')
    return(1)
  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {
    message(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
    return(1)
  }
  
  # check id returns 0 if the id column was unique
  if(check_id(object, list(key)) == 0) {    
    sub_obj <- substitute(object)
    if (is.name(sub_obj)) 
      sub_obj <- deparse(sub_obj)
    parent <- parent.frame()
    if (exists(sub_obj, envir = parent, inherits = TRUE)) 
      object@key <- list(key)
    else {
      stop('variable not defined')
    }
    #assign(sub_obj, object, envir = .GlobalEnv)
    assign(sub_obj, object, envir = parent)
    return(0)
  } else {
    message(c(key, " column was not unique"))
    return(1)
    
  }
  
})