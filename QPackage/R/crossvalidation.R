cv_kfold <- function(feat_table, num_folds, model, ...)
  
  if(!is_labeled_table(feat_table)) {
    stop("cv kfold expects feature vector with labels as the first arguement")
  }
  keys <- unlist(feat_table@key)

  # machine learning models don't like NAs
  feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set

  x <- subset(feat_table, select = - c(keys, "label"))
  y <- as.factor(feat_table[, "label"])

  folds <- caret::createFolds(nrow(feat_table), k = num_folds) # default value of k is 10

  #todo
  #for each fold
  #   train model using training set
  #   predict using validation set
  #   compute accuracy
  #end
  # return average accuracy


  
}