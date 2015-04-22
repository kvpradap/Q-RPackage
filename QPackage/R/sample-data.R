sample_qtable <- function(table_obj, num_samples, replace = FALSE) {
  if(!is.qtable(table_obj)) {
    stop("sample_qtable is implemented for qtable")
  }
  key <- table_obj@key
  sample_df <- table_obj[sample(1:nrow(table_obj), num_samples, replace = replace), ]
  sample_df <- as.data.frame(sample_df)
  row.names(sample_df) <- NULL
  table_obj <- qtable(sample_df)
  set_id(table_obj, key)
  return(table_obj)
}