#' Export qtable to csv
#' 
#' write_csv exports qtable object to csv file. It uses \code{\link{write.csv}}
#' to write to csv file.
#' 
#' @param table_obj qtable object to be written to csv file.
#' @param ... Relevant parameters to \code{\link{write.csv}}
#' 
#' @examples 
#' 
#' \dontrun{
#' write_csv(qtable_obj, file = "qtable.csv")
#' }
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