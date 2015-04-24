apply_block <- function(table_a, table_b, func_obj, ..., col_names_a = NULL, col_names_b = NULL) {
  
  fn_name <- deparse(substitute(func_obj))
  if(fn_name == "attr_equiv_block") {
    cand_set <- attr_equiv_block(table_a, table_b, ..., col_names_a, col_names_b)
    return(cand_set)
  } else {
    stop("Only built-in attribute equivalence blocker is supported")
    
  }
    
}