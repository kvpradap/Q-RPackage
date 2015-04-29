write_csv <- function(table_obj, ...) {
  res_status <- 0
  if(!is_qtable(table_obj)) {
    message("Input table is expected to be a qtable")
    res_status <- 1
  } else {
    x <- data.frame(table_obj)
    write.csv(x, ...)
  }
  
  return(res_status)
  
}