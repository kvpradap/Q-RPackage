apply_block <- function(table_a, table_b, func_obj, ..., col_names_a = "NULL", col_names_b = "NULL") {
  res_vec = c()
  
  for(i in 1:nrow(table_a)) {
    t1 <- table_a[i, ]
    for(j in 1:nrow(table_b)) {          
      t2 <- table_b[j, ]
      res <- func_obj(t1, t2, ...) 
      res_vec <- c(res_vec, res)
    }
  }
  return(res_vec)
  
}