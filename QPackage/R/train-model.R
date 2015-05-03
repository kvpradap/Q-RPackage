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

#' Train model
#' 
#' Train supervised learning model using features from labeled data.
#' 
#' @param feat_table Feature vectors represented as qtable,  computed from labeled data. 
#' @param method Learning model to be applied represented as string. Currently 
#'   seven learning models are supported  (see \code{\link{show_learners}}). The
#'   method should be one of the following: "svm", "rpart", "randomforest",
#'   "nnet", "bagging", "boosting", "naivebayes".
#' 
#' @param ... Optional parameters relevant to learning model.
#'   
#' @return Returns trained model.
#'   
#' @examples
#' 
#' \dontrun{
#'  dt_model <- train_model(labeled_feat_vec, method = "rpart")
#' }
#' 
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