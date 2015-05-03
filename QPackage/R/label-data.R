#' Label data
#' 
#' Lets the user provide labeled data that can be used for model selection, 
#' training and prediction. \code{label_data} opens a GUI with data from 
#' candidate set and an extra column named "label". The user can manually update
#' the labels with values 0 to indicate that the tuple pairs donot match and 1 
#' to indicate that the tuple pairs match.
#' 
#' @param sampled_data Sampled candidate set represented as qtable.
#'   
#' @return qtable object with columns from input qtable along with "label"
#'   column updated by the user.
#'   
#' @examples
#' 
#' 
#' \dontrun{
#' labeled_Data <- label_data(sampled_data)
#' }
#'   
label_data <- function(sampled_data) {
  if(!is_qtable(sampled_data)) {
    stop("label data is implemented for qtable")
  }
  
  if("label" %in% names(sampled_data)) {
    stop("Input qtable already contains label attribute")
  }
  
  key <- sampled_data@key
  
  # add a new column
  labeled_data <- cbind(sampled_data, data.frame(label = rep(0, nrow(sampled_data))))
  row.names(labeled_data) <- NULL
  
  # opens up GUI for the user to edit
  labeled_data <- edit(labeled_data)
  
  # bundling it in a qtable and returning back
  labeled_data <- qtable(labeled_data)
  set_id(labeled_data, key)
  
  return(labeled_data)
}