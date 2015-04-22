arrange_col_names <- function(n, p,  block_attr1, block_attr2 = NULL) {
  if(!is.null(block_attr2)) {
    if(n %in% block_attr2 == TRUE) {
      i <- grep(n, block_attr2)
      n <- block_attr1[i]
      return(n)
    }
  }

  if(n %in% block_attr1) {
    return(n)
  }

  return(paste(n,p, sep=""))

}


attr_equiv_block <- function(table_a, table_b, block_attr1, block_attr2, col_list_a = NULL, col_list_b = NULL) {
  
  if(is.qtable(table_a) == FALSE) {
    stop("Input table A should be of qtable type")
  }
  if(is.qtable(table_b) == FALSE) {
    stop("Input table A should be of qtable type")
  }
  tbl_a_ids <- unlist(table_a@key)
  tbl_b_ids <- unlist(table_b@key)
  
  
  if(length(tbl_a_ids) == 0) {
    stop("Input table A does not have key set")
  }
  
  if(length(tbl_b_ids) == 0) {
    stop("Input table B does not have key set")
  }
  
  
  if(!is.null(col_list_a)) {
    if( all(unlist(col_list_a) %in% names(table_a)) == FALSE) {
      stop("Optional columns from table A does not match with actual column names from A")
    }
  }
  
  if(!is.null(col_list_b)) {
    if( all(unlist(col_list_b) %in% names(table_b)) == FALSE) {
      stop("Optional columns from table B does not match with actual column names from B")
    }
  }
  
  cand_set <- merge(table_a, table_b, by.x = block_attr1, by.y = block_attr2)
  
  
  # ------ retain only required columns --------------------

  retain_col_names <- c(paste(tbl_a_ids, ".x", sep=""), paste(tbl_b_ids, ".y", sep=""))
  
  # get the id col names and massage for key to be set
  id_col_names <- c(paste("A.", tbl_a_ids, sep=""), paste("B.", tbl_b_ids, sep=""))
  fin_names <- id_col_names
  
  if(!is.null(col_list_a)) {
    col_list_a <- unlist(col_list_a)
    # remove ids from optional column names
    col_list_a <- setdiff(col_list_a, tbl_a_ids)
    # get the optional column names and massage to names that I expect to see in cand set
    col1_names <-  lapply(col_list_a, arrange_col_names, ".x", block_attr1)
    fin_col1_names <- paste("A.", col_list_a, sep="")
    fin_names <- c(fin_names, fin_col1_names)
    retain_col_names <- c(retain_col_names, col1_names)
  }
  
  if(!is.null(col_list_b)) {
    col_list_b <- unlist(col_list_b)
    col_list_b <- setdiff(col_list_b, tbl_b_ids)
    col2_names <-  lapply(col_list_b, arrange_col_names, ".y", block_attr1, block_attr2)
    fin_col2_names <- paste("B.", col_list_b, sep="")
    fin_names <- c(fin_names, fin_col2_names)    
    retain_col_names <- c(retain_col_names, col2_names)    
  }
  
 cand_set <- cand_set[, unlist(retain_col_names)]
 names(cand_set) <- fin_names
 cand_set <- qtable(cand_set)
 set_id(cand_set, list(id_col_names))
 return(cand_set)
}






# attr_equiv_block <- function(tuple_a, tuple_b, block_attr1, block_attr2) {
#   
#   if(any(colnames(tuple_a) == block_attr1) == FALSE) {
#     message(c("tuple_a", " does not contain ", block_attr1))
#     return(FALSE)
#   }
#   
#   if(any(colnames(tuple_b) == block_attr2) == FALSE) {
#     message(c("tuple_b", " does not contain ", block_attr2))
#     return(FALSE)
#   }
#   
#   if(tuple_a[block_attr1] == tuple_b[block_attr2]) {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
#   
#   
# }