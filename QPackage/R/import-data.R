#' Import csv file into QTable

read_csv_basic <- function (...) {
  #----- load csv file into dataframe
  df <- read.csv(..., stringsAsFactors=F)
  #---- create qtable object
  q <- QTable(df)
  
  return(q)  
  
}



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
    if (check_id(q, key) == TRUE) {
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










read_csv_stat <- function(..., key = list(), status) {
  
  # load csv file into data frame
  df <- read.csv(..., stringsAsFactors = FALSE)
  # create qtable object
  q <- qtable(df)
  
  
  #-----------------------
  
  subx <- substitute(status)
  if (is.name(subx)) 
    subx <- deparse(subx)
  if (!is.character(subx)) 
    stop("'function' requires a name")
  parent <- parent.frame()
  #assign(subx, x, envir = parent)
  
  #----------------------
  
  
  
  # handle key column 
  
  # -- case 1 : key column is empty list
  if (length(key) == 0) {
    if (exists(subx, envir = parent, inherits = TRUE)) 
      status <- 10
    else {
      stop('variable not defined')
      #status <- 35
      #environment(status) <- parent
    }
    
    #assign(sub_obj, object, envir = .GlobalEnv)
    assign(sub_x, status, envir = parent)
    
    return(q)
  }
  
  # -- case 2 : key column has list of column names
  if (check_id(q, key) == TRUE) {
    if (exists(subx, envir = parent, inherits = TRUE)) 
      status <- 10
    else {
      stop('variable not defined')
      #status <- 35
      #environment(status) <- parent
      
      
    }
    
    
    set_id(q, key)
  } else {
    # -------- set the status 
    if (exists(subx, envir = parent, inherits = TRUE)) 
      status <- 10
    else {
      stop('variable not defined')
      #status <- 35
      #environment(x) <- parent
      
      
    }
    #assign(sub_obj, object, envir = .GlobalEnv)
    
    
  }
  assign(subx, status, envir = parent)
  return(q)
  
  
  
}
ReadCsv <- function (..., idCol = NA) {
  
  q <- read_csv_basic(...)
  
  #---- handle idCol
  
  #-- case 1: idCol is NA
  if (is.na(idCol) == TRUE) {
    return(q)
  }
  
  #-- case 2: idCol is column name
  if (is.character(idCol) == TRUE) {
    if (CheckId(q, idCol) == TRUE) {
      # set id attribute
      q <- SetId(q, idCol)
    } else {
      # print warning
      warning("IdCol contains non-unique values")      
    }
    return(q)
  }
  
  #-- case 3: idCol is column index
  if (is.integer(idCol) == TRUE) {
    
    #-- Get column name
    colName <- colnames(q)
    
    if (CheckId(q, idCol) == TRUE) {
      # set id attribute
      q <- SetId(q, idCol)
    } else {
      # print warning
      warning("IdCol contains non-unique values")      
    }
    return(q)
  }
  
}