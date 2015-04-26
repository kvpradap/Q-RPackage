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