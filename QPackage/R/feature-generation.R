create_features <- function() {
  
  fn_list <- list()
  
  fn_str <-  fill_fn_template("publisher", "publisher", "lev")
  fn_list[["lev_brand"]] <- eval(parse(text = fn_str))
  
  fn_str <-  fill_fn_template("title", "title", "jaccard", "tok_qgram", 2,"tok_qgram", "2")
  fn_list[["jac_title_qgram2_title_qgram_2"]] <- eval(parse(text = fn_str))
  
  fn_str <- fill_fn_template("title", "title", "jaccard", "tok_whitespace", NULL,"tok_whitespace", NULL)
  fn_list[["jac_title_ws_title_ws"]] <- eval(parse(text=fn_str))
  
  fn_str <- fill_fn_template("pages", "pages", "diff_vals")
  fn_list[["diff_price"]] <- eval(parse(text=fn_str))
                              
  return(fn_list)
  
  
}



# function that creates a feature function based on the inputs
fill_fn_template <- function(attr1, attr2, simfunction, tok1 = NULL, tok1param = NULL, tok2 = NULL, tok2param = NULL) {
  fn_st <- "fn <- function(t1, t2) {"
  
  fn_body <- "res <- "
  
  if(!is.null(tok1) & !is.null(tok2)) {
    fn_body <- paste0(fn_body, simfunction, "( ", tok1, "(", "t1$", attr1)
    if(!is.null(tok1param)) {
      fn_body <- paste0(fn_body, ", ",  tok1param, "), ");
    } else {
      fn_body <- paste0(fn_body, "), ");
    }
    
    fn_body <- paste0(fn_body, tok2, "(", "t2$", attr2)
    if(!is.null(tok2param)) {
      fn_body <- paste0(fn_body, ", ",  tok2param, ")) ");
    } else {
      fn_body <- paste0(fn_body, ")) ");
    }
    
  } else {
    fn_body <- paste0(fn_body, simfunction, "( t1$", attr1, ", t2$", attr2, ") \n")
  }
  
  fn_en <- "; return(res)}"
  
  fn <- paste0(fn_st, fn_body, fn_en)
  
  return(fn)
}

# function to convert types of columns
convert_types <- function(df,classes){
  out <- lapply(1:length(classes),
                FUN = function(classIndex){as(df[,classIndex],classes[classIndex])})
  names(out) <- colnames(df)
  return(data.frame(out, stringsAsFactors = FALSE))
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
  ret_df <- convert_types(ret_df, rep("numeric", 4))
  
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

