createFolds <- function (y, k = 10, list = TRUE, returnTrain = FALSE) 
{
  if (is.numeric(y)) {
    cuts <- floor(length(y)/k)
    if (cuts < 2) 
      cuts <- 2
    if (cuts > 5) 
      cuts <- 5
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = cuts))), 
             include.lowest = TRUE)
  }
  if (k < length(y)) {
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    for (i in 1:length(numInClass)) {
      seqVector <- rep(1:k, numInClass[i]%/%k)
      if (numInClass[i]%%k > 0) 
        seqVector <- c(seqVector, sample(1:k, numInClass[i]%%k))
      foldVector[which(y == dimnames(numInClass)$y[i])] <- sample(seqVector)
    }
  }
  else foldVector <- seq(along = y)
  if (list) {
    out <- split(seq(along = y), foldVector)
    names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), 
                        sep = "")
    if (returnTrain) 
      out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
  }
  else out <- foldVector
  out
}

# cv_kfold <- function(feat_table, num_folds, model, model_args = list(), predict_args = list()) {
#   
#   if(!is_labeled_table(feat_table)) {
#     stop("cv kfold expects feature vector with labels as the first argument")
#   }
#   keys <- unlist(feat_table@key)
#   
#   feat_table <- data.frame(feat_table)
# 
#   # machine learning models don't like NAs or NaNs
#   feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set
#   #feat_table <- rapply( feat_table, f=function(x) ifelse(is.nan(x),0,x), how="replace" )
#   
#   
#   #feat_table <- data.frame(feat_table)
#   feat_table[, "label"] <- as.factor(feat_table[, "label"])
#   x <- feat_table[ , -which(names(feat_table) %in% keys)]
#   x_p <- x[, -which(names(x) %in% "label")]
#   
#   
#   
# 
#   folds <- createFolds(1:nrow(feat_table), k = num_folds) # default value of k is 10
#   
#   num_correct <- c()
#   num_total <- c()
#   
#   for(i in 1:num_folds) {
#     train_data <- x[ -folds[[i]], ]
#     test_data <- x_p[ folds[[i]], ]
#     # The final model args must contain the formula and  train data set. 
#     # The optional model args are concatenated to formula and dataset.
#     concat_model_args <- c(list(label ~ ., train_data), model_args)
#     train_model <- do.call(model, concat_model_args)
# 
#     # The final model args must contain the trained model and  test data set. 
#     # The optional predict args are concatenated to trained model and test data
#     
#     concat_predict_args <- c(list(train_model, test_data), predict_args)
#     
#     predictions <- do.call(predict, concat_predict_args)
#     
#     predictions <- as.data.frame(predictions)
#     
#     num_correct <- c(num_correct,  sum(predictions[, 1] == x[folds[[i]], "label"]))
#     num_total <- c(num_total, length(folds[[i]]))
#   }
# ##   print(num_total)
# ##  print(num_correct)
# #   print(sum(num_correct))
# #   print(sum(num_total))
#   accuracy <- sum(num_correct)/sum(num_total)
#   
#   return(accuracy)
#   
# }

#' K-fold cross validation
#' 
#' \code{cv_kfold} computes k-fold crossvalidation given feature vector(got from
#' labeled data) and learning model.
#' 
#' @param feat_table Feature vector (got from labeled data) represented as 
#'   qtable object.
#' @param num_folds Number of folds.
#' @param method Learning model to be applied represented as string. Currently 
#'   seven learning models are supported  (see \code{\link{show_learners}}). The
#'   method should be one of the following: "svm", "rpart", "randomforest",
#'   "nnet", "bagging", "boosting", "naivebayes".
#'   
#'   
#' @param ... Optional parameters relevant to learning model.
#'   
#' @return Returns average accuracy computed from k-fold corssvalidation.
#'   
#' @examples
#' 
#' \dontrun{
#'  acc_dt <- cv_kfold(feat_table, 10, method="rpart")
#' }

cv_kfold <- function(feat_table, num_folds, method = NULL, ...) {
  if(!is_labeled_table(feat_table)) {
    stop("cv kfold expects feature vector with labels as the first argument")
  }
  if(is.null(method)) {
    stop("Learning method is not specified")
  }
  method <- as.character(method)
  
  
  
  keys <- unlist(feat_table@key)
  
  feat_table <- data.frame(feat_table)
  
  
  # machine learning models don't like NAs or NaNs
  feat_table[is.na(feat_table)] <- 0 # need to revisit on what value should be set
  
  feat_table[, "label"] <- as.factor(feat_table[, "label"])
  x <- feat_table[ , -which(names(feat_table) %in% keys)]
  x_p <- x[, -which(names(x) %in% "label")]
  
  folds <- createFolds(1:nrow(feat_table), k = num_folds) # default value of k is 10
  
  num_correct <- c()
  num_total <- c()
  
  
  
  for(i in 1:num_folds) {
    train_data <- x[ -folds[[i]], ]
    test_data <- x_p[ folds[[i]], ]
    
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
    predictions <- switch(as.character(method),
                          svm = predict(model, newdata = test_data),
                          rpart = predict(model, newdata = test_data, type = "class"),
                          randomforest = predict(model, newdata = test_data, type = "response"),
                          nnet = predict(model, newdata = test_data, type = "class"),
                          bagging = predict(model, newdata = test_data, type = "class"),
                          boosting = predict(model, newdata = test_data, type = "vector"),
                          naivebayes = predict(model, newdata = test_data, type = "class"),
                          stop("Illegal method")
                  )
    
    predictions <- as.data.frame(predictions)
    
    predictions <- factor(predictions[, "predictions"], levels = c(0, 1))
    
    num_correct <- c(num_correct,  sum(predictions == x[folds[[i]], "label"]))
    num_total <- c(num_total, length(folds[[i]]))
  }
  
  accuracy <- sum(num_correct)/sum(num_total)
  
  return(accuracy)
  
}