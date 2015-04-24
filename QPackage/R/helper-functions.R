is.qtable <- function(table_obj) {
  # get the class
  cls <- class(table_obj)
  cls <- cls[1]
  if(cls == "qtable")
    return(TRUE)
  else
    return(FALSE)
}