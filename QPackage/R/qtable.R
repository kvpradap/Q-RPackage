qtable <- function(...) {
  q <- new ("qtable", data.frame(...))
  # attr(q, "key") <- NULL
}

setClassUnion("list.or.NULL", c("list", "NULL"))

setClass(
  "qtable", slots=c(key="list.or.NULL"),
  contains = "data.frame"
)

setGeneric("check_id", function(object, key) standardGeneric("check_id"))

setGeneric("set_id", function(object, key) standardGeneric("set_id"))



setMethod("check_id", representation(object="qtable", key="list"), function(object, key) {
  
  key <- unlist(key)
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    message('key can be only a character vector')
    return(FALSE)
  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {
    message(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
    return(FALSE)
  }
  
  # check unique values in key
  uniq_vals <- unique(object[key])
  if( nrow(uniq_vals) != nrow(object) ) {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

setMethod("set_id", representation(object="qtable",  key="list"), function(object, key) {
  
  key <- unlist(key)
  
  if(length(key) == 0) {
    message('key is empty')
    return(FALSE)
  }
  
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    message('key can be only a character vector')
    return(FALSE)
  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {
    message(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
    return(FALSE)
  }

  if(check_id(object, list(key)) == TRUE) {    
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
    return(TRUE)
  } else {
    message(c(key, " column was not unique"))
    return(FALSE)
    
  }
  
})