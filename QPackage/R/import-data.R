#' Import csv file into qtable
#' 
#' Reads a file in table format and creates a qtable from it. It uses \code{\link{read.csv}} 
#' to read the contents of csv file.
#' 
#' @param ... Relavant parameters to \code{\link{read.csv}}.
#' @param key (optional) List of attribute names that can be set as key.
#' @param status (optional) Symbol to store return status. 
#' 
#' @examples
#' 
#' \dontrun{
#' bowker_table <- read_csv("bowker.csv", 
#'                  key = list("id"), 
#'                  status = read_status)
#' }

read_csv <- function(..., key = NULL, status = NULL) {
  # load csv file into data frame
  df <- read.csv(..., stringsAsFactors = FALSE)
  # create qtable object
  q <- qtable(df)
  
  
  # handle key column   
  # -- case 1 : key column is empty list
  if (is.null(key)) {      
    ret_status <- 0
  } else {
    # -- case 2 : key column has list of column names
    if (check_id(q, key) == 1) {
      set_id(q, key)
      ret_status <- 0
    } else {
      ret_status <- 1
    }
  }
  
  # --- set the status -- start
  sub_x <- substitute(status)
  if(!is.null(sub_x)) {  
    if (is.name(sub_x)) {
      sub_x <- deparse(sub_x)
    }
    if (!is.character(sub_x)) {
      stop("status requires a name")
    }
    parent <- parent.frame()
    if (!exists(sub_x, envir = parent, inherits = FALSE)) {
      environment(sub_x) <- parent
    }
    assign(sub_x, ret_status, envir = parent)
  }
  # --- set the status -- done !!!
  return(q)
}










