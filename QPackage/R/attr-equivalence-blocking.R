
# function creates inverted index from column values to ids
create_inv_index <- function(table_obj, col_name) {
  
  
  id_col <-  table_obj@idCol
  
  # function to grep sthg in a string
  # http://stackoverflow.com/questions/14494217/how-can-i-build-an-inverted-index-from-a-data-frame-in-r
  l <- function(z) {
    # grep a string in list
    indices <- grep(z, table_obj[, col_name])
    
    table_obj[indices, id_col]
  }
  lv <- Vectorize(l)
  
  unique_vals <- unique(table_obj[, col_name])
  y <- lv(unique_vals)  
  
  setNames(y, unique_vals)
  
}

# function to get candidate pairs

get_pairs <- function(inv_index) {
  
  # consider inv index with # ids > 1
  inv_index <- inv_index[lapply(tt, length) > 1]
  
  # 
  
   
}