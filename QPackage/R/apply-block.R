#' Apply blocking
#' 
#' It takes in two qtables and a blocking function. It applies the blocking 
#' function to tuple pairs from input table and return a candidate set with 
#' potentially matching tuple pairs.
#' 
#' @param table_a, table_b Input qtables, table A and table B.
#' @param func_obj Blocking function to be applied. It is a R function which is 
#'   expected to take two tuples (and other parameters) and return 1 if they 
#'   match and 0 if they dont. Note: Currently if the function object's name is 
#'   \code{attr_equiv_block}, the parameters are attribute name from table A to 
#'   be matched with attribute name from table B. The blocking function is not 
#'   invoked for every tuple pair, instead a join based on attributes is done.
#' @param ... Parameters relevant to blocking function. For 
#'   \code{attr_equiv_block} it is the attribute name from table A and table B. 
#'   Currently blocking based on single attribute is supported.
#' @param col_names_a (optional) List of column names from table A to be included in the
#'   candidate set.
#' @param col_names_b (optional) List of column names from table B to be incldued in the
#'   candidate set.
#' @return A candidate set represented as qtable object. The candidate set 
#'   contains the following: \code{id} attributes from table A and table B, 
#'   optional columns as mentioned in col_names_a, optional columns as mentioned
#'   in col_names_b.
#'   @examples 
#'   
#'   \donotrun{
#'   attr1 < "isbn"
#'   attr2 <- "isbn"
#'   cand_set <- apply_block(walmart, bowker, attr_equiv_block, attr1, attr2, coln)
#'   
#'   }
apply_block <- function(table_a, table_b, func_obj, ..., col_names_a = NULL, col_names_b = NULL) {
  
  fn_name <- deparse(substitute(func_obj))
  if(fn_name == "attr_equiv_block") {
    cand_set <- attr_equiv_block(table_a, table_b, ..., col_names_a, col_names_b)
    return(cand_set)
  } else {
    stop("Only built-in attribute equivalence blocker is supported")
    
  }
    
}