cv_kfold <- function(feat_table, num_folds, model, model_args = list(), predict_args = list()) {
  
  if(!is_labeled_table(feat_table)) {
    stop("cv kfold expects feature vector with labels as the first argument")
  }
  keys <- unlist(feat_table@key)
  
  feat_table <- data.frame(feat_table)

  # machine learning models don't like NAs
  feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set

  
  
  feat_table[, "label"] <- as.factor(feat_table[, "label"])
  x <- feat_table[ , -which(names(feat_table) %in% keys)]
  x_p <- x[, -which(names(x) %in% "label")]
  
  
  

  folds <- caret::createFolds(1:nrow(feat_table), k = num_folds) # default value of k is 10
  
  num_correct <- c()
  num_total <- c()
  
  for(i in 1:num_folds) {
    train_data <- x[ -folds[[i]], ]
    test_data <- x_p[ folds[[i]], ]
    # The final model args must contain the formula and  train data set. 
    # The optional model args are concatenated to formula and dataset.
    concat_model_args <- c(list(label ~ ., train_data), model_args)
    train_model <- do.call(model, concat_model_args)

    # The final model args must contain the trained model and  test data set. 
    # The optional predict args are concatenated to trained model and test data
    
    concat_predict_args <- c(list(train_model, test_data), predict_args)
    
    predictions <- do.call(predict, concat_predict_args)
    
    predictions <- as.data.frame(predictions)
    
    num_correct <- c(num_correct,  sum(predictions[, 1] == x[folds[[i]], "label"]))
    num_total <- c(num_total, length(folds[[i]]))
  }
#   print(num_total)
#   print(num_correct)
#   print(sum(num_correct))
#   print(sum(num_total))
  accuracy <- sum(num_correct)/sum(num_total)
  
  return(accuracy)

  
}