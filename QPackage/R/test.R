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
# temp <- 5
# ref <- function (temporari) 
# {
#   subx <- substitute(temporari)
#   if (is.name(subx)) 
#     subx <- deparse(subx)
#   if (!is.character(subx)) 
#     stop("'function' requires a name")
#   parent <- parent.frame()
#   if (exists(subx, envir = parent, inherits = TRUE)) 
#     x <- 10
#   else {
#     #stop('variable not defined')
#     x <- 35
#     environment(x) <- parent
#   
#   
#   }
#   assign(subx, x, envir = parent)
# }
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



# fn <- function(n, p,  block_attr1, block_attr2) {
#   if(n %in% block_attr2 == TRUE) {
#     i <- grep(n, block_attr2)
#     n <- block_attr1[i]
#     return(n)
#   }
#   
#   if(n %in% block_attr1) {
#     return(n)
#   }
#   
#   return(paste(n,p, sep=""))
#   
# }



if(0) {
library(rJava)
  .jinit()
  .jaddClassPath("/scratch/pradap/r-work/simfunction.jar")
  .jaddClassPath("/scratch/pradap/local/share/javalibs/simmetrics_jar_v1_6_2_d07_02_07.jar")
  obj <- .jnew("build/SimilarityFunction")
  res_tokens <- .jrcall(obj, "whitespaceTokenizer", "m uru gas ara")
  print(res_tokens)
  res_single_string <- .jrcall(obj, "getSimilarity", "mur", "muru")
  print(res_single_string)
  res <- .jrcall(obj, "getSimilarity", res_tokens, res_tokens)
}

if(0) {
fn <- function(a, b) { return(2*a + b)}
assign("jac_1", fn)



get_fn_list <- function() {
  list_fns <- list()
  
  
  fn <- function(a, b) { return(a +b)}
  assign(deparse(substitute(s)), fn)
  #list_fns[[1]] <- as.symbol(deparse(substitute(s)))
  list_fns[["jac_1"]] <- fn
  
  s <- "jac_2"
  fn <- function(a, b) { return(a - b)}
  assign(deparse(substitute(s)), fn)
  #list_fns[[2]] <- as.symbol(deparse(substitute(s)))
  list_fns[["jac_2"]] <- fn
  return(list_fns)
}
eval(parse(text = "fn <- function(a, b) { return (a * b) } "))

}
