# -- import data
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

if(0) {
  t1  <- tuple("brand" = "Draper", "title" = "Draper Infrared Remote Transmitter", "price" = 58.45)
  t2  <- tuple("brand" = "Drapr", "title" = "Draper Infrared Remote Tx", "price" = 68.45)
  fn_list <- auto_feat_gen()
  apply_fn <- function(fn, t1, t2) {
    #print(t1)
    return(fn(t1, t2))
  }
  lapply(fn_list, apply_fn, t1, t2)
}


  apply_fn <- function(fn, t1, t2) {
    #print(t1)
    return(fn(t1, t2))
  }
  
  myfun <- function(t, col_types) {
    print(t)
    # need two steps to get a data.frame format
    t <- as.list(t)
    t <- as.data.frame(t, stringsAsFactors = FALSE)

    t <- convert_magic2(t, col_types)
    
    return(t)
  }

  fn_1 <- function(table_a, table_b, fn_list) {
    merged_d <- cbind(table_a, table_b)
    col_names_a <- colnames(table_a)
    col_names_b <- colnames(table_b)
    
    # apply sends each row character vector to FUN. We need to preserve the types of the columns
    # so we send the coltypes to FUN
    col_types <- as.vector(sapply(merged_d, class))
    ret <- apply(merged_d, 1, fn_2, col_types, col_names_a, col_names_b, fn_list)
    return(ret)
  }

  
  convert_types <- function(df,classes){
    out <- lapply(1:length(classes),
                  FUN = function(classIndex){as(df[,classIndex],classes[classIndex])})
    names(out) <- colnames(df)
    return(data.frame(out, stringsAsFactors = FALSE))
  }


  fn_2 <- function(merged_tuple, col_types, col_names_a, col_names_b, fn_list) {
    # convert input tuple s.t the column types are preserved

    merged_tuple <- as.list(merged_tuple)
    merged_tuple <- as.data.frame(merged_tuple, stringsAsFactors = FALSE)
    
    merged_tuple <- convert_types(merged_tuple, col_types)
    
    
    l1 <- length(col_names_a)
    l2 <- length(col_names_b)
    
    t1 <- merged_tuple[, 1:l1]
    t2 <- merged_tuple[, (1:l2) + l1]
  
    
    ret_list <- lapply(fn_list, apply_fn, t1, t2)
    return(ret_list)
                
    
  }    

if(0) {
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  
  
  merge.with.order <- function(x,y, ..., sort = T, keep_order = 1)
  {
    # took it from : http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    
    
    # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
    add.id.column.to.data <- function(DATA)
    {
      data.frame(DATA, id... = seq_len(nrow(DATA)))
    }
    # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
    order.by.id...and.remove.it <- function(DATA)
    {
      # gets in a data.frame with the "id..." column.  Orders by it and returns it
      if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
      
      ss_r <- order(DATA$id...)
      ss_c <- colnames(DATA) != "id..."
      DATA[ss_r, ss_c]
    }
    
    # tmp <- function(x) x==1; 1  # why we must check what to do if it is missing or not...
    # tmp()
    
    if(!missing(keep_order))
    {
      if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
      if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
      # if you didn't get "return" by now - issue a warning.
      warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
    } else {return(merge(x=x,y=y,..., sort = sort))}
  }
  
  keys <- unlist(cand_set@key)
  tbl_a_idx <- grep("A.", keys)
  
  if(!is.integer0(tbl_a_idx)) {
    tbl_a_key <- keys[tbl_a_idx]
    #cset_a <- merge(walmart, cand_set, by.x = unlist(walmart@key), by.y = tbl_a_key)
    cset_a <- merge.with.order(walmart, cand_set, by.x = unlist(walmart@key), by.y = tbl_a_key, keep_order = 2)
  
  
  
  }
  tbl_b_idx <- grep("B.", keys)
  
  if(!is.integer0(tbl_b_idx)) {
    tbl_b_key <- keys[tbl_b_idx]
    #cset_b <- merge(bowker, cand_set, by.x = unlist(walmart@key), by.y = tbl_b_key)
    cset_b <- merge.with.order(bowker, cand_set, by.x = unlist(bowker@key), by.y = tbl_b_key, keep_order = 2)
  }
  cset_a_small <- cset_a[1:5, ]
  cset_b_small <- cset_b[1:5, ]
  fn_list <- create_features()
  feat_vec <- apply_feat_fn_over_2tables(cset_a_small, cset_b_small, fn_list)
  
  
  
  #-----------
  merge.with.order <- function(x,y, ..., sort = T, keep_order = 1)
  {
    # took it from : http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    
    
    # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
    add.id.column.to.data <- function(DATA)
    {
      data.frame(DATA, id... = seq_len(nrow(DATA)))
    }
    # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
    order.by.id...and.remove.it <- function(DATA)
    {
      # gets in a data.frame with the "id..." column.  Orders by it and returns it
      if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
      
      ss_r <- order(DATA$id...)
      ss_c <- colnames(DATA) != "id..."
      DATA[ss_r, ss_c]
    }
    
    # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
    # tmp()
    
    if(!missing(keep_order))
    {
      if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
      if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
      # if you didn't get "return" by now - issue a warning.
      warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
    } else {return(merge(x=x,y=y,..., sort = sort))}
  }

}
# ----

if(0) {
  word_len <- function(s) {
    length(unlist(strsplit(s, " ")))
  }
  check_valid <- function(s, simlist, toklist) {
    s <- unlist(s)
    if(any(s %in% simlist)) {
      return(NULL)
    }
    if(any(s %in% toklist))
      return(NULL)
    return(s)
  }
  fill_fn <- function(s, l) {
    l <- unlist(l)
    s <- unlist(s)
    
    s <- list(s)
    l <- list(l)
    print(s)
    print(l)
    fn_str <- do.call(fill_fn_template, c(l, s))
    fn_obj <- eval(parse(text = fn_str))
  }
  
  a <- list()
  b <- list()
   b[[1]] <- list("lev")
   b[[2]] <- list("jaccard", "tok_whitespace", NULL, "tok_whitespace", NULL)
   b[[3]] <- list("jaccard", "tok_qgram", 3, "tok_qgram", 3)
  a$mws <- b

}

if(FALSE) {
  w <- walmart_bk_dataset
  b <- bowker_bk_dataset
  
  attr_corres = NULL
  if(is.null(attr_corres)) {
    c1 <- colnames(w)
    c2 <- colnames(b)
    cmn <- setdiff(intersect(c1, c2), c(unlist(w@key), unlist(b@key)))
    attr_corres <- lapply(cmn, rep, 2)    
  }
  
  global_simlist <- c("jaccard, lev")
  global_toklist <- c("tok_whitespace", "tok_gram")
  
  allowed_simlist <- c("jaccard", "lev")
  allowed_toklist <- c( "tok_qgram")
  
  # for loop start
  attrs <- attr_corres[[1]]
  if(class(w[, attrs[1]]) == class(w[, attrs[2]])) {
    if(class(w[, attrs[1]]) == "character") {
      len1 <- lapply(w[, attrs[1]], word_len)
      len1 <- unlist(len1)
      len2 <- lapply(w[, attrs[2]], word_len)
      len2 <- unlist(len1)
      if(mean(len1) > 5 && mean(len2) > 5) {
        mws <- a$mws
        avoid_simlist <- setdiff(global_simlist, allowed_simlist)
        avoid_toklist <- setdiff(global_toklist, allowed_toklist)
        valid_list <- lapply(mws, check_valid, avoid_simlist, avoid_toklist)
        valid_list <- valid_list[!sapply(valid_list, is.null)]
        attr_list <- lapply(attrs, id_fn)
        fn_objs <- lapply(valid_list, fill_fn, attr_list)
        
      }
    }
  }
  
  # for loop end
  
}
if(FALSE) {
  lkp_table <- list()
  l1 <- list()
  l1[[1]] <- list("monge_elkan")
  l1[[2]] <- list("jaccard", "tok_whitespace", NULL, "tok_whitespace", NULL)
  l1[[3]] <- list("cosine", "tok_whitespace", NULL, "tok_whitespace", NULL)
  
  lkp_table$MWM <- l1
  
  l1 <- list()
  l1[[1]] <- list("jaccard", "tok_whitespace", NULL, "tok_whitespace", NULL)
  l1[[2]] <- list("cosine", "tok_whitespace", NULL, "tok_whitespace", NULL)
  
  lkp_table$MWL <- l1
  
  l1 <- list()
  l1[[1]] <- list("monge_elkan")
  l1[[2]] <- list("jaccard", "tok_whitespace", NULL, "tok_whitespace", NULL)
  l1[[3]] <- list("jaccard", "tok_qgram", 3, "tok_qgram", 3)
  l1[[4]] <- list("needleman_wunsch")
  l1[[5]] <- list("smith_waterman_gotoh")
  l1[[6]] <- list("smith_waterman")
  l1[[7]] <- list("cosine", "tok_whitespace", NULL, "tok_whitespace", NULL)
  
  lkp_table$MWS <- l1
  
  l1 <- list()
  l1[[1]] <- list("jaro")
  l1[[2]] <- list("jaro_winkler")
  l1[[3]] <- list("lev")
  l1[[4]] <- list("soundex")
  l1[[5]] <- list("jaccard", "tok_qgram", 3, "tok_qgram", 3)
  l1[[6]] <- list("exact_match")
  
  lkp_table$SWS <- l1
  
  l1 <- list()
  l1[[1]] <- list("abs_diff")
  l1[[2]] <- list("rel_diff")
  l1[[3]] <- list("lev")
  l1[[4]] <- list("exact_match")
  
  lkp_table$NUM <- l1
  auto_gen_table... <- lkp_table
  
  dict <- list()
  dict["jaccard"] <- "jac"
  dict["tok_whitespace"] <- "ws"
  dict["tok_qgram"] <- "qgm"
  dict["lev"] <- "lev"
  dict["cosine"] <- "cos"
  dict["monge_elkan"] <- "mel"
  dict["needleman_wunsch"] <- "nmw"
  dict["smith_waterman"] <- "sw"
  dict["smith_waterman_gotoh"] <- "swg"
  dict["jaro"] <- "jar"
  dict["jaro_winkler"] <- "jwn"
  dict["soundex"] <- "sdx"
  dict["exact_match"] <- "exm"
  dict["abs_diff"] <- "adf"
  dict["rel_diff"] <- "rdf"
  dict["1"] <- "1"
  dict["2"] <- "2"
  dict["3"] <- "3"
  dict["4"] <- "4"
  
  
  feat_lkp_names... <- dict
  
  
  
  gbl_sim_fns <- c("jaccard", "lev", "cosine", "monge_elkan", "needleman_wunsch", "smith_waterman", "smith_waterman_gotoh", "jaro", "jaro_winkler", "soundex", "exact_match", "abs_diff", "rel_diff")
  
  gbl_toks <- c("tok_whitespace", "tok_qgram")
  
  gbl_sim_fns... <- gbl_sim_fns
  gbl_toks... <- gbl_toks
  
  
  id_fn <- function(inp) {
    inp
  }
  fill_fn <- function(s, l) {
    fn_str <- do.call(fill_fn_template, c(l, s))
    fn_obj <- eval(parse(text = fn_str))
    return(fn_obj)
    
  }
  get_names <- function(s, attrs) {
    dict <- list()
    dict["jaccard"] <- "jac"
    dict["tok_whitespace"] <- "ws"
    dict["tok_qgram"] <- "qgm"
    dict["lev"] <- "lev"
    paste0(attrs, dict[s], sep="", collapse="_")
  }
}