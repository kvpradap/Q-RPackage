

# create_features <- function() {
#   
#   fn_list <- list()
#   
#   fn_str <-  fill_fn_template("publisher", "publisher", "lev")
#   fn_list[["lev_pub"]] <- eval(parse(text = fn_str))
#   
#   fn_str <-  fill_fn_template("title", "title", "jaccard", "tok_qgram", 2,"tok_qgram", "2")
#   fn_list[["jac_title_qgram2_title_qgram_2"]] <- eval(parse(text = fn_str))
#   
#   fn_str <- fill_fn_template("title", "title", "jaccard", "tok_whitespace", NULL,"tok_whitespace", NULL)
#   fn_list[["jac_title_ws_title_ws"]] <- eval(parse(text=fn_str))
#   
#   fn_str <- fill_fn_template("pages", "pages", "diff_vals")
#   fn_list[["diff_pages"]] <- eval(parse(text=fn_str))
#   
#   return(fn_list)
#   
#   
# }

# function that creates a feature function based on the inputs
fill_fn_template <- function(attr1, attr2, simfunction, tok1 = NULL, tok1param = NULL, tok2 = NULL, tok2param = NULL) {
  fn_st <- "fn <- function(t1, t2) {"
  
  fn_body <- "res <- "
  
  if(!is.null(tok1) & !is.null(tok2)) {
    fn_body <- paste0(fn_body, simfunction, "( ", tok1, "(", "t1$", attr1)
    if(!is.null(tok1param)) {
      fn_body <- paste0(fn_body, ", ",  tok1param, "), ");
    } else {
      fn_body <- paste0(fn_body, "), ");
    }
    
    fn_body <- paste0(fn_body, tok2, "(", "t2$", attr2)
    if(!is.null(tok2param)) {
      fn_body <- paste0(fn_body, ", ",  tok2param, ")) ");
    } else {
      fn_body <- paste0(fn_body, ")) ");
    }
    
  } else {
    fn_body <- paste0(fn_body, simfunction, "( t1$", attr1, ", t2$", attr2, ") \n")
  }
  
  fn_en <- "; return(res)}"
  
  fn <- paste0(fn_st, fn_body, fn_en)
  
  return(fn)
}

#' Create features
#' 
#' Automatically create features based on input qtable's attribute type and its 
#' statistics.
#' 
#' @param table_a,table_b Input qtable objects.
#' @param attr_corres List of character vectors capturing the attribute 
#'   correspondence between table_a and table_b. If set to NULL, attribute 
#'   correspondence is done based on attribute names.
#' @param allowed_simlist List of strings containing the similarity function 
#'   names (a subset of global similarity function list) to be considered for 
#'   automatic feature creation. If set to NULL, global similarity function list
#'   (see \code{\link{show_simfns}}) is considered.
#' @param allowed_toklist List of strings containing the tokenizer names (a 
#'   subset of global tokenizer list) to be considered for automatic feature 
#'   creation. If set to NULL, global tokenizer list (see 
#'   \code{\link{show_tokenizers}}) is considered.
#'   
#' @return Returns a list of features represented as R functions that takes in
#'   two tuples and return a numeric value.
#'   
#' @examples
#' 
#' \dontrun{
#' feature_list <- create_features(walmart, bowker, 
#'                  attr_corres = list(c("title", "title),c("binding", "binding")),
#'                  allowed_sim_list = list("jaccard", "lev"),
#'                  allowed_tok_list = list("tok_qgram")
#'                  )
#' }


create_features <- function(table_a, table_b, attr_corres = NULL, allowed_simlist = NULL, allowed_toklist = NULL) {
  if(is.null(attr_corres)) {
    c1 <- colnames(table_a)
    c2 <- colnames(table_b)
    cmn <- setdiff(intersect(c1, c2), c(unlist(table_a@key), unlist(table_b@key)))
    attr_corres <- lapply(cmn, rep, 2)
  }
  
  fn_obj_list <- list()
  for(i in 1:length(attr_corres)) {
    attrs <- attr_corres[[i]]
    # check if their classes are same
    if(mode(table_a[, attrs[1]]) == mode(table_b[, attrs[2]])) {
      if(mode(table_a[, attrs[1]]) == "character") {
        len1 <- lapply(table_a[, attrs[1]], num_words)
        len1 <- unlist(len1)
        m1 <- mean(len1, na.rm = TRUE) # to be safe ignore NAs
        
        len2 <- lapply(table_b[, attrs[2]], num_words)
        len2 <- unlist(len2)
        m2 <- mean(len2, na.rm = TRUE) # to be safe ignore NAs
        
        if(m1 == 1 && m2 == 1) {
          # Single word string
          fn_obj <- get_fn_objs(attrs, QPackage:::auto_gen_table...$SWS, allowed_simlist, allowed_toklist)
          fn_obj_list <- c(fn_obj_list, fn_obj)
        } else if(m1 <= 5 && m2 <= 5) {
          fn_obj <- get_fn_objs(attrs, QPackage:::auto_gen_table...$MWS, allowed_simlist, allowed_toklist)
          fn_obj_list <- c(fn_obj_list, fn_obj)
        } else if(m1 <= 10 && m2 <= 10) {
          fn_obj <- get_fn_objs(attrs, QPackage:::auto_gen_table...$MWM, allowed_simlist, allowed_toklist)
          fn_obj_list <- c(fn_obj_list, fn_obj)          
        } else if(m1 >= 11 && m2 >= 11) {
          fn_obj <- get_fn_objs(attrs, QPackage:::auto_gen_table...$MWL, allowed_simlist, allowed_toklist)
          fn_obj_list <- c(fn_obj_list, fn_obj)          
          
        }
        # end of character attr type
      } else if(mode(table_a[, attrs[1]]) == "numeric") {
        fn_obj <- get_fn_objs(attrs, QPackage:::auto_gen_table...$NUM, allowed_simlist, allowed_toklist)
        fn_obj_list <- c(fn_obj_list, fn_obj)        
      }
    }
  }
  return(fn_obj_list)
  
}

# ----- helper functions
# get number of words in a string
num_words <- function(s) {
  length(unlist(strsplit(s, " ")))
}

get_fn_objs <- function(attrs, rec_fns, allowed_simlist, allowed_toklist) {
  allowed_simlist <- unlist(allowed_simlist)
  allowed_toklist <- unlist(allowed_toklist)
  avoid_simlist <- c()
  avoid_toklist <- c()
  
  if(length(allowed_simlist) > 0)
    avoid_simlist <- setdiff(QPackage:::gbl_sim_fns..., allowed_simlist)
  
  if(length(allowed_toklist) > 0)
    avoid_toklist <- setdiff(QPackage:::gbl_toks..., allowed_toklist)
  
  # remove heuristics that involve simfns and tokenizers in the black list
  valid_list <- lapply(rec_fns, check_valid, avoid_simlist, avoid_toklist)
  
  # remove null
  #valid_list <- valid_list[!sapply(valid_list, is.null)]
  
  attr_list <- lapply(attrs, id_fun)
  
  fn_objs <- lapply(valid_list, fill_fn, attr_list)
  fn_objs <- fn_objs[!sapply(fn_objs, is.null)]
    
    
  nms <- lapply(valid_list, get_names, attrs)
  # The reason why am I not pruning NULL earlier is, valid_list can contain nested NULLs and I dont to get them pruned
  nms <- nms[!sapply(valid_list, is.null)]

  names(fn_objs) <- nms
  
  return(fn_objs)
  
  
}
# does not do anything. I need it as it want every element in the vector to be wrapped in a list
id_fun <- function(inp) {
  inp
}

# check if the recommened function involves blacklisted sim functions or tokenizers
check_valid <- function(inp, simlist, toklist) {
  upd_inp <- unlist(inp)
  if(any(upd_inp %in% simlist)) {
    return(NULL)
  }
  if(any(upd_inp %in% toklist))
    return(NULL)
  return(inp)
}

# create the function object
fill_fn <- function(inp, l) {
  if(is.null(inp))
    return(NULL)
  fn_str <- do.call(fill_fn_template, c(l, inp))
  fn_obj <- eval(parse(text = fn_str))
}

# get names for the function objects
get_names <- function(inp, attrs) {
  if(is.null(inp))
    return(NULL)
  inp <- unlist(inp)
  a1 <- paste0(attrs, sep="", collapse="_")
  a2 <- paste0(QPackage:::feat_lkp_names...[inp], sep="", collapse="_")
    
  paste0(a1, "_", a2, sep="", collapse = "_")  
}



