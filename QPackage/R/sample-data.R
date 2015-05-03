#' Sample qtable
#' 
#' \code{sample_table} It takes a sample of specified size from the qtable using
#' either with or without replacement.
#' 
#' @param table_obj qtable object
#' @param num_samples a positive number, the number of items to choose. Note:
#'   currently only random sampling is supported
#' @param replace boolean to do sampling with or without replacement.
#'   
#' @return qtable with subset of rows of size \code{num_samples} from input
#'   table
#'   
#' @examples
#' 
#' \dontrun{
#' sampled_qtable <- sample_qtable(qtable_obj, 25)
#' }
#'   
sample_qtable <- function(table_obj, num_samples, replace = FALSE) {
  if(!is_qtable(table_obj)) {
    stop("sample_qtable is implemented for qtable")
  }
  key <- table_obj@key
  
  # do random sampling of the input table
  sample_df <- table_obj[sample(1:nrow(table_obj), num_samples, replace = replace), ]
  #sample_df <- as.data.frame(sample_df)
  #row.names(sample_df) <- NULL
  
  # bundle it in qtable and return
  table_obj <- qtable(sample_df)
  set_id(table_obj, key)
  return(table_obj)
}