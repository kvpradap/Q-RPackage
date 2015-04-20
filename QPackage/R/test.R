# -- import data
#<<<<<<< HEAD
# d <- ReadCsv('../QPackage/data/DBLP_cleaned.csv', idCol="id")
# 
# d_sample = QTable(d[1:10, ])
# 
# d_sample = SetId(d_sample, d@idCol)
# 
# # inv_index <- create_inv_index(d_sample, "year")
# # 
# # inv_index <- inv_index[lapply(inv_index, length) > 1]
# # 
# # qtable <- function(df) {
# #   structure(list(df, key=c()), class=c("foo", "data.frame"))
# # }
temp <- 5
ref <- function (temporari) 
{
  subx <- substitute(temporari)
  if (is.name(subx)) 
    subx <- deparse(subx)
  if (!is.character(subx)) 
    stop("'function' requires a name")
  parent <- parent.frame()
  if (exists(subx, envir = parent, inherits = TRUE)) 
    x <- 10
  else {
    #stop('variable not defined')
    x <- 35
    environment(x) <- parent
  
  
  }
  assign(subx, x, envir = parent)
}
# =======
# d <- ReadCsv('../QPackage/data/DBLP_cleaned.csv', idCol="id")
# 
# d_sample <- QTable(d[1:10, ])
# 
# d_sample <- SetId(d_sample, d@idCol)

#inv_index <- create_inv_index(d_sample, "year")

#inv_index <- inv_index[lapply(inv_index, length) > 1]

#cand_set <- get_pairs(inv_index)
#df <- block_attr_equivalence(d_sample, "year")






