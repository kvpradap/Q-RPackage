fn <- function(d) {
  df <- data.frame()
  
  r <- combn(d, 2, FUN=function(x) { df <<- rbind(x, df) })
  #df <- rbind(c(10,11), df)
  #df <- rbind(c(11,12), df)
  colnames(df) <- c("id1", "id2")
  #print(df)
  df
}
#df = data.frame()
#print(fn(inv_index[[2]]))
#print(df)

fn1 <- function(indx) {
  str_opt = getOption("stringsAsFactors")
  #options(stringsAsFactors = FALSE)
  options(stringsAsFactors = F)
  #df1 <- data.frame()
  kk <- lapply(indx, FUN=fn)
  kk <- unname(kk)
  
  df <- data.frame()
  
  for(i in 1:length(kk)) {
    df <- rbind(df, kk[[i]])
  }
  
  options(stringsAsFactors=str_opt)
  
  df
}


# fn1 <- function(tl) {
#   df <- data.frame()
#   #colnames(df) = c("id1", "id2")
#   #r <- lapply(tl, combn, 2, FUN=function(x) { print(x) })
#   l <- function(x) {
#     df1 <- data.frame()
#     #colnames(df1) = c("id1", "id2")
#     #res <- combn(x, 2, FUN=function(y) { rbind(df1, as.character(y))})
#     
#     xx <- ""
#     res <- combn(x, 2, FUN=function(y) { df1 <- rbind(df1, (y))})
#     print(xx)
#     print(df1)
#     rbind(df, df1)
#   }
#   r <- lapply(tl, FUN= l)
#   df
#   
#   
# } 