#' Import csv file into QTable

ReadCSV <- function (..., idCol=NA) {
  #----- load csv file into dataframe
  df <- read.csv(...)
  
  #---- create qtable object
  q <- QTable(df)
  
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