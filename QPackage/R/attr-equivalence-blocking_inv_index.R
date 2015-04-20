
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

# helper function to get_pairs 
# given a vector, computes n choose 2 pairs and return it as data frame
fn <- function(d) {
  df <- data.frame()
  r <- combn(d, 2, FUN=function(x) { df <<- rbind(x, df) })
  colnames(df) <- c("id1", "id2")
  df
}

# function to get candidate pairs

get_pairs <- function(inv_index) {
  
  # consider inv index with # ids > 1
  inv_index <- inv_index[lapply(inv_index, length) > 1]
  
  # store current setting of stringsAsFactors 
  str_opt <- getOption("stringsAsFactors")
  options(stringsAsFactors = F)
  
  df_idx <- lapply(inv_index, FUN=fn)
  kk <- unname(df_idx)
  
  df <- data.frame()
  
  # merge all the dataframes
  for(i in 1:length(kk)) {
    df <- rbind(df, kk[[i]])
  }
  
  # reset back stringsAsFactors option
  options(stringsAsFactors=str_opt)
  
  df
  
}

block_attr_equivalence <- function(table_obj, col_name) {
  if(nchar(table_obj@idCol) == 0) {
    stop("id col is empty")
  }
  inv_index <- create_inv_index(table_obj, col_name)
  cand_set <- get_pairs(inv_index)
}