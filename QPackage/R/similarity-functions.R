jaccard <- function(s1, s2) {
  
  
  if(is.null(s1) | is.null(s2)) {
    return(0) 
  }

  if(any(is.na(s1)) | any(is.na(s2))) {
    return(NA)
  }
  
  if(is.list(s1)) {
    s1 <- unlist(s1)
  }
  if(is.list(s2)) {
    s2 <- unlist(s2)
  }
  
  # call java code
  sim_obj <- .jnew("build/SimilarityFunction")
  sim_val <- .jcall(sim_obj,"D", "jaccard", s1, s2)
  
  return(sim_val)
  
}

lev <- function(s1, s2) {
  if(is.null(s1) | is.null(s2)) {
    return(0) 
  }
  if(is.na(s1) | is.na(s2)) {
    return(NA)
  }
  
  if(!is.character(s1)) {
    s1 <- as.character(s1)
  }
  if(!is.character(s2)) {
    s2 <- as.character(s2)
  }
  
  
  # call java code
  sim_obj <- .jnew("build/SimilarityFunction")
  sim_val <- .jcall(sim_obj,"D", "levenshtein", s1, s2)
  
  return(sim_val)
  
}

# numeric similarities
ndiff_vals <- function(d1, d2) {
  return(abs( (d1-d2)/max(d1, d2)))
}

diff_vals <- function(d1, d2) {
  return(abs(d1 - d2))
}

