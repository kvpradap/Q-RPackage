attr_equiv_block <- function(tuple_a, tuple_b, block_attr1, block_attr2) {
  
  if(any(colnames(tuple_a) == block_attr1) == FALSE) {
    message(c("tuple_a", " does not contain ", block_attr1))
    return(FALSE)
  }
  
  if(any(colnames(tuple_b) == block_attr2) == FALSE) {
    message(c("tuple_b", " does not contain ", block_attr2))
    return(FALSE)
  }
  
  if(tuple_a[block_attr1] == tuple_b[block_attr2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  
}