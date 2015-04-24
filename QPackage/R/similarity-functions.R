jaccard <- function(s1, s2) {
  
  # if one them is NA return NA
  if(length(s1) > 0) {
    if(any(is.na(s1))) {
      return(NA)
    }
  }

  if(length(s2) > 0) {
    if(any(is.na(s2))) {
      return(NA)
    }
  }
  
  # if one of them is null return 0
  if(is.null(s1) | is.null(s2)) {
    return(0) 
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
  
  if(length(s1) > 0) {
    if(any(is.na(s1))) {
      return(NA)
    }
  }
  
  if(length(s2) > 0) {
    if(any(is.na(s2))) {
      return(NA)
    }
  }
  
  if(is.null(s1) | is.null(s2)) {
    return(0) 
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
  
  if(length(d1) > 0) {
    if(is.na(d1)) {
      return(NA)
    }
  }
  
  if(length(d2) > 0) {
    if(is.na(d2)) {
      return(NA)
    }
  }
  if(is.null(d1) | is.null(d2)) {
    return(0)
  }
  
  
  return(abs( (d1-d2)/max(d1, d2)))
}

diff_vals <- function(d1, d2) {
  if(length(d1) > 0) {
    if(is.na(d1)) {
      return(NA)
    }
  }
  
  if(length(d2) > 0) {
    if(is.na(d2)) {
      return(NA)
    }
  }
  if(is.null(d1) | is.null(d2)) {
    return(0)
  }
  return(abs(d1 - d2))
}

