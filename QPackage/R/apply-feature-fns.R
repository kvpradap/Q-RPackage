
# Using input tables, apply list of feature functions over either 
# (i) labeled data that has label as the last column
# (ii) candidate set

#' Convert to feature vectors
#' 
#' Given base tables, labeled data (or candidate set) and feature list (R
#' function). \code{convert_to_feature_vec} computes the features by joining
#' candidate set to base tables and applying features represented as R
#' functions.
#' 
#' @param table_a,table_b Base tables represented as qtable objects.
#' @param cand_set Labeled data (or candidate set) represented as qtable.
#' @param fn_list List of features represented as R functions.
#'   
#' @return Returns a qtable with feature values.
#'   
#' @examples
#' 
#' \dontrun{
#'  labeled_feat_vec <- convert_to_feature_vecs(walmart, bowker,
#'                        labeled_data,
#'                        feat_list
#'                      )
#' }
convert_to_feature_vecs <- function(table_a, table_b, cand_set, fn_list) {
  
  # basic checks
  if(!is_qtable(table_a)) {
    stop('Table A is not a qtable')
  }
  if(!is_qtable(table_b)) {
    stop('Table B is not a qtable')
  }
  if(!is_qtable(cand_set)) {
    stop('candidate set (labeled/unlabeled) is not a qtable')
  }

  # get candidate set key 
  keys <- unlist(cand_set@key)
  if(length(keys) < 2) {
    stop('candidate set (labeled/unlabeled) is expected to have composite key of size atleast 2 ')
  }
  lbl_flag <- is_labeled_table(cand_set)
  
  
  
  if(lbl_flag) {
    # grab the label column
    lbl_col <- cand_set[, "label"]
    num_cols <- length(colnames(cand_set))
    
    # remove the label col from labeled set
    cand_set <- cand_set[, seq(1, num_cols-1)]
    
  }
  
  
  tbl_a_idx <- grep("A.", keys)
  
  if(!is_integer0(tbl_a_idx)) {
    tbl_a_key <- keys[tbl_a_idx]
    # Order in which I do the merge is important, because merge is going to retain column ids from first table for 
    # the attributes used for attribute equivalence
    # 
    cset_a <- merge.with.order(table_a, cand_set, by.x = unlist(table_a@key), by.y = tbl_a_key, keep_order = 2)

  }
  tbl_b_idx <- grep("B.", keys)
  
  if(!is_integer0(tbl_b_idx)) {
    tbl_b_key <- keys[tbl_b_idx]
    cset_b <- merge.with.order(table_b, cand_set, by.x = unlist(table_b@key), by.y = tbl_b_key, keep_order = 2)
  }
  
  feat_vec <- apply_feat_fn_over_2tables(cset_a, cset_b, fn_list)
  
  feat_vec <- cbind(cand_set[, keys], feat_vec)
  if(lbl_flag) {
    feat_vec <- cbind(feat_vec, "label"=lbl_col)
  }
  feat_vec <- qtable(feat_vec)
  set_id(feat_vec, list(keys))
  return(feat_vec)
  
  
  
}

apply_feat_fn_over_2tables <- function(table_a, table_b, fn_list) {
  
  # combine the tables - stack them side by side 
  merged_table <- cbind(table_a, table_b)
  
  # get column names and types
  col_names_a <- colnames(table_a)
  col_names_b <- colnames(table_b)
  col_types <- as.vector(sapply(merged_table, class))
  
  # apply function over the merged (combined) tuple
  ret <- apply(merged_table, 1, apply_feat_fn_over_merged_tuple_pair, col_types, col_names_a, col_names_b, fn_list)
  
  
  
  # convert list of lists to dataframe
  ret_df <- do.call(rbind, ret)
  ret_df <- convert_types(ret_df, rep("numeric", ncol(ret_df)))
  
  return(ret_df)
}



# Give a tuple as merged tuple, apply a list of feature functions and return a list.
# The function does the following
# (1) Set the merged tuple's types matching original input tables.
# (2) Split the tuple (based on length of given column names)
# (3) Apply a list of functions over two tuples
apply_feat_fn_over_merged_tuple_pair <- function(merged_tuple, col_types, col_names_a, col_names_b, fn_list) {
  # convert input tuple s.t the column types are preserved
  
  merged_tuple <- as.list(merged_tuple)
  merged_tuple <- as.data.frame(merged_tuple, stringsAsFactors = FALSE)
  
  merged_tuple <- convert_types(merged_tuple, col_types)
  
  
  l1 <- length(col_names_a)
  l2 <- length(col_names_b)
  
  t1 <- merged_tuple[, 1:l1]
  t2 <- merged_tuple[, (1:l2) + l1]
  
  # trick here is apply_fn_2tuples will get the function that should be applied and all that it does is
  # wrap the function and call it over two tuples
  ret_list <- lapply(fn_list, apply_feat_fn_2tuples, t1, t2)
  return(ret_list)
  
  
}    

# apply function over two tuples
apply_feat_fn_2tuples <- function(fn, t1, t2) {
  #print(t1)
  return(fn(t1, t2))
}

