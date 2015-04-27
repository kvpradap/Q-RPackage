predict_label <- function(feat_table, model, predict_args = list()) {
  if(!is_qtable(feat_table)) {
    stop("predict model expects qtable containing feature vectors as the first argument")
  }
  keys <- unlist(feat_table@key)
  
  feat_table_cp <- feat_table
  
  feat_table_cp <- data.frame(feat_table_cp)
  
  # machine learning models don't like NAs
  feat_table_cp[is.na(feat_table)] <- 0
  
  test_data <- feat_table_cp[ , -which(names(feat_table_cp) %in% keys)]
  concat_predict_args <- c(list(model, test_data), predict_args)
  
  predicted_labels <- do.call(predict, concat_predict_args)
  predicted_labels <- as.data.frame(predicted_labels)
  
  out <- cbind(data.frame(feat_table), predicted_labels )
  out <- qtable(out)
  set_id(out, list(keys))
  
  return(out)
}