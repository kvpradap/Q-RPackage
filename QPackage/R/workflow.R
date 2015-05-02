# import files ----------------------------------------------- start

# -- walmart
#read_files <- function() {
if(FALSE) {
  print("Importing walmart data")
  walmart <- read_csv("../QPackage/inst//extdata/books/walmart.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(walmart, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(walmart, list("id"))
  print(status)
  
  
  # -- bowker
  print("Importing bowker data")
  bowker <- read_csv("../QPackage/inst//extdata/books/bowker.csv", status = read_status)
  print(read_status)
  
  # check id
  status <- check_id(bowker, list("id"))
  print(status)
  
  
  # set id
  status <- set_id(bowker, list("id"))
  print(status)
#}

}
# View data ----------------------------------------------- start
#View(bowker)
#View(walmart)

# ---- blocking

#do_block <- function(walmart, bowker, attr1, attr2) {
if(FALSE) {
attr1 <- "isbn"
attr2 <- "isbn"
cand_set <-  apply_block(walmart, bowker, attr_equiv_block, attr1, attr2, col_names_a = list("title", "author", "binding", "publisher", "pages"), col_names_b = list("title", "author", "binding", "publisher", "pages"))
#}

# ---- sample data set
sampled_data <- sample_qtable(cand_set, 25)
  
# ---- label data set
labeled_data <- label_data(sampled_data)

# --- create features
#feature_list <- create_features(walmart, bowker, list(c("title", "title"), c("numAuthors", "numAuthors"), c("binding", "binding")))
feature_list <- create_features(walmart, bowker)


# --- compute feature vectors

labeled_feat_vec <- convert_to_feature_vecs(walmart, bowker, labeled_data, feature_list)

# --- model selection
# --- compute accuracy for different models with CV using labeled data

# Models compared : rpart, randomForest, SVM
# do 10 fold cross validation

library(rpart)

acc_dt <- cv_kfold(labeled_feat_vec, 10, method = "rpart")
#acc_dt <- cv_kfold(labeled_feat_vec, 10, rpart, predict_args = list(type = "class"))

library(randomForest)
acc_rf <- cv_kfold(labeled_feat_vec, 10, method = "randomforest")
#acc_rf <- cv_kfold(labeled_feat_vec, 10, randomForest)

library(e1071)
acc_svm <- cv_kfold(labeled_feat_vec, 10 , method = "svm")
#acc_svm <- cv_kfold(labeled_feat_vec, 10, svm)





# The user chooses randomForest to train on whole labeled data
model <- train_model(labeled_feat_vec, method = "randomforest")

# Use the trained model to predict labels for candidate set
candset_feat_vec <- convert_to_feature_vecs(walmart, bowker, cand_set, feature_list)
candset_fv_with_labels<- predict_label(candset_feat_vec, model)

View(merge(cand_set,candset_fv_with_labels))


}