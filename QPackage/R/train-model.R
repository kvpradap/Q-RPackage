# train_model <- function(feat_table,  model, model_args = list()) {
#   if(!is_labeled_table(feat_table)) {
#     stop("train model feature vector with labels as the first argument")
#   }
#   keys <- unlist(feat_table@key)
#   
#   feat_table <- data.frame(feat_table)
#   
#   # machine learning models don't like NAs
#   feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set
#   
#   
#   
#   feat_table[, "label"] <- as.factor(feat_table[, "label"])
#   train_data <- feat_table[ , -which(names(feat_table) %in% keys)]
#   concat_model_args <- c(list(label ~ ., train_data), model_args)
#   train_model <- do.call(model, concat_model_args)
#   return(train_model)
#   
# }

train_model <- function(feat_table, method = NULL, ...) {
  
    if(!is_labeled_table(feat_table)) {
      stop("train model feature vector with labels as the first argument")
    }
    keys <- unlist(feat_table@key)
    
    feat_table <- data.frame(feat_table)
    
    # machine learning models don't like NAs
    feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set
  
    feat_table[, "label"] <- as.factor(feat_table[, "label"])
    train_data <- feat_table[ , -which(names(feat_table) %in% keys)]
    
    model <- switch(as.character(method),
                    svm = e1071::svm(label ~ ., data = train_data, type = "C-classification", ...),
                    rpart = rpart::rpart(label ~ ., data = train_data, method = "class", ...),
                    randomforest = randomForest::randomForest(label ~ ., data = train_data, ...),
                    nnet = nnet::nnet(label ~ ., data = train_data, size = 2, ...),
                    bagging = ipred::bagging(label ~ ., data = train_data, ...),
                    boosting = ada::ada(label ~ ., data = train_data, ...),
                    naivebayes = e1071::naiveBayes(label ~ ., data = train_data, ...),
                    stop("Illegal method")
    )
    
    return(model)
  
}