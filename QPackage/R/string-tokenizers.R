# white space tokenizer
tok_whitespace <- function(s) {
  # expect it to be a simple string
  # basic checks !!!
  if(is.null(s)) {
    return(NULL)
  }
  if(is.na(s))
    return(NA)

  if(length(s) > 1) {
    message("Tokenizer takes in a simple string not a vector")
    return(NULL)
  }
  if(is.list(s)) {
    message("Tokenizer takes in a simple string not a list")
    return(NULL)
  }
  # leave it to java tokenizer to handle it
#   if(nchar(s) == 0) {
#     return(NULL)
#   }
  
  
  # call java code !!!
  tokenizer <- .jnew("build/Tokenizers")
  res_tokens <- .jcall(tokenizer, "[S", "whitespaceTokenizer", s)
  
  # return result
  return(res_tokens)
}

tok_qgram <- function(s, q) {
  # basic checks !!!
  
  if(is.null(s)) {
    return(NULL)
  }
  
  if(is.na(s))
    return(NA)
  
  
  if(length(s) > 1) {
    message("Tokenizer takes in a simple string not a vector")
    return(NULL)
  }
  if(is.list(s)) {
    message("Tokenizer takes in a simple string not a list")
    return(NULL)
  }
  
  # call java code !!!
  tokenizer <- .jnew("build/Tokenizers")
  res_tokens <- .jcall(tokenizer, "[S", "qgramTokenizer", s, q)
  
  # return result
  return(res_tokens)
  
} 