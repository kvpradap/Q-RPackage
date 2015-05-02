# predict_label <- function(feat_table, model, predict_args = list()) {
#   if(!is_qtable(feat_table)) {
#     stop("predict model expects qtable containing feature vectors as the first argument")
#   }
#   keys <- unlist(feat_table@key)
#   
#   feat_table_cp <- feat_table
#   
#   feat_table_cp <- data.frame(feat_table_cp)
#   
#   # machine learning models don't like NAs
#   feat_table_cp[is.na(feat_table)] <- 0
#   
#   test_data <- feat_table_cp[ , -which(names(feat_table_cp) %in% keys)]
#   concat_predict_args <- c(list(model, test_data), predict_args)
#   
#   predicted_labels <- do.call(predict, concat_predict_args)
#   predicted_labels <- as.data.frame(predicted_labels)
#   
#   out <- cbind(data.frame(feat_table), predicted_labels )
#   out <- qtable(out)
#   set_id(out, list(keys))
#   
#   return(out)
# }


predict_label <- function(feat_table, model) {
  
  if(!is_qtable(feat_table)) {
    stop("predict model expects qtable containing feature vectors as the first argument")
  }
  keys <- unlist(feat_table@key)
  
  feat_table_cp <- feat_table
  
  feat_table_cp <- data.frame(feat_table_cp)
  
  # machine learning models don't like NAs
  feat_table_cp[is.na(feat_table)] <- 0
  
  test_data <- feat_table_cp[ , -which(names(feat_table_cp) %in% keys)]
  
  method = get_model_name(model)
  
  predicted_labels <- switch(as.character(method),
                             svm = predict(model, newdata = test_data),
                             rpart = predict(model, newdata = test_data, type = "class"),
                             randomforest = predict(model, newdata = test_data, type = "response"),
                             nnet = predict(model, newdata = test_data, type = "class"),
                             bagging = predict(model, newdata = test_data, type = "class"),
                             boosting = predict(model, newdata = test_data, type = "vector"),
                             naivebayes = predict(model, newdata = test_data, type = "class"),
                             stop("Illegal method")
  )
  
  predicted_labels <- as.data.frame(predicted_labels)
  out <- cbind(data.frame(feat_table), predicted_labels )
  out <- qtable(out)
  set_id(out, list(keys))
  
  return(out)
  
  
}

get_model_name <- function(model) {
  # get the class name
  cls <- class(model)
  # if there are two names pick the second one
  if(length(cls) == 2) {
    cls <- cls[2]
  }
  
  model_name <- switch(cls,
                       svm = "svm",
                       rpart = "rpart",
                       randomForest = "randomforest",
                       nnet = "nnet",
                       classbagg = "bagging",
                       ada = "boosting",
                       naiveBayes = "naivebayes"
  )
  return(model_name)
  
  
  
}