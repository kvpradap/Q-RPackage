auto_feat_gen <- function() {
  
  fn_list <- list()
  
  fn_str <-  fill_fn_template("brand", "brand", "lev")
  fn_list[["lev_brand"]] <- eval(parse(text = fn_str))
  
  fn_str <-  fill_fn_template("title", "title", "jaccard", "tok_qgram", 2,"tok_qgram", "2")
  fn_list[["jac_title_qgram2_title_qgram_2"]] <- eval(parse(text = fn_str))
  
  fn_str <- fill_fn_template("title", "title", "jaccard", "tok_whitespace", NULL,"tok_whitespace", NULL)
  fn_list[["jac_title_ws_title_ws"]] <- eval(parse(text=fn_str))
                              
  return(fn_list)
  
  
}

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