label_data <- function(sampled_data) {
  if(!is.qtable(sampled_data)) {
    stop("label data is implemented for qtable")
  }
  
  if("label" %in% names(sampled_data)) {
    stop("Input qtable already contains label attribute")
  }
  
  key <- sampled_data@key
  
  labeled_data <- cbind(sampled_data, data.frame(label = rep(0, nrow(sampled_data))))
  
  row.names(labeled_data) <- NULL
  
  labeled_data <- edit(labeled_data)
  
  labeled_data <- qtable(labeled_data)
  set_id(labeled_data, key)
  
  return(labeled_data)
}