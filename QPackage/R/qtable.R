qtable <- function(..., key=list()) {
  new ("qtable", data.frame(...), key=list())
}

setClass(
  "qtable", slots=c(key="list"),
  contains = "data.frame"
)

setGeneric("check_id", function(object, key) standardGeneric("check_id"))

setGeneric("set_id", function(object, key, status) standardGeneric("set_id"))

setMethod("check_id", representation(object="qtable", key="list"), function(object, key) {
  
  key <- unlist(key)
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    error('key can be only a character vector')
    return(false)
  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {
    error(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
  }
  
  # check unique values in key
  uniq_vals <- unique(object[key])
  if( nrow(uniq_vals) != nrow(object) ) {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

setMethod("set_id", representation(object="qtable",  key="list", status="numeric"), function(object, key, status) {
  
  key <- unlist(key)
  # check if the input vector is of character type
  if(is.character(key) == FALSE) {
    #updating parent env
    # -----------------------------
    
    
    
    # -----------------------------
    
    error('key can be only a character vector')

  }
  
  # check if the key attr names are present in qtable
  if(all(key %in% colnames(object)) == FALSE) {

    #updating parent env
    status <<- 1

    error(c('key [', key,  ']does not form a subset of qtable column names [ ', colnames(object)), ' ]')
  }

  if(check_id(object, list(key)) == TRUE) {
    #object@key <<- list(key)
    # -----------------------------
    
    sub_obj <- substitute(object)
    if (is.name(sub_obj)) 
      sub_obj <- deparse(sub_obj)
    parent <- parent.frame()
    if (exists(sub_obj, envir = parent, inherits = TRUE)) 
      object@key <- list(key)
    else {
      stop('variable not defined')
    }
    assign(sub_obj, object, envir = .GlobalEnv)
    
    
    # updating status
    # -----------------------------
    print("after setting obj")
    sub_status <- substitute(status)
    if (is.name(sub_status)) {
      sub_status <- deparse(sub_status)
    }
    parent <- parent.frame()
    if (exists(sub_status, envir = parent, inherits = TRUE)) 
      status <- 0
    else {
      
      status <- 0
      environment(status) <- .GlobalEnv
    }
    assign(sub_status, status, envir = .GlobalEnv)
    
    # --------------------------- 
    
    #return(object)
  } else {
    message(c(key, " column was not unique"))
    
    #updating parent env
    # -----------------------------
    sub_status <- substitute(status)
    if (is.name(sub_status)) {
      sub_status <- deparse(sub_status)
    }
    parent <- parent.frame()
    if (exists(sub_status, envir = parent, inherits = TRUE)) 
      status <- 0
    else {
      status <- 0
      environment(status) <- .GlobalEnv
    }
    assign(sub_status, status, envir = .GlobalEnv)
    # ---------------------------------------------
    #return(object)
  }
  
})