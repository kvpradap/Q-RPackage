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
  
  
  # trick to match array type required at java side
  if(length(s1)) {
    s1 <- rep(s1, 2)
  }
  
  # trick to match array type required at java side
  if(length(s2)) {
    s2 <- rep(s2, 2)
  }
  
  # call java code
  sim_obj <- .jnew("build/SimilarityFunction")
  sim_val <- .jcall(sim_obj,"D", "jaccard", s1, s2)
  
  return(sim_val)
  
}

cosine <- function(s1, s2) {
  
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
  
  
  # trick to match array type required at java side
  if(length(s1)) {
    s1 <- rep(s1, 2)
  }
  
  # trick to match array type required at java side
  if(length(s2)) {
    s2 <- rep(s2, 2)
  }
  
  # call java code
  sim_obj <- .jnew("build/SimilarityFunction")
  sim_val <- .jcall(sim_obj,"D", "cosine", s1, s2)
  
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


jaro <- function(s1, s2) {
  
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
  sim_val <- .jcall(sim_obj,"D", "jaro", s1, s2)
  
  return(sim_val)
  
}

jaro_winkler <- function(s1, s2) {
  
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
  sim_val <- .jcall(sim_obj,"D", "jaroWinkler", s1, s2)
  
  return(sim_val)
  
}

soundex <- function(s1, s2) {
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
  sim_val <- .jcall(sim_obj,"D", "soundex", s1, s2)
  
  return(sim_val)
  
}

monge_elkan <- function(s1, s2) {
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
  sim_val <- .jcall(sim_obj,"D", "mongeElkan", s1, s2)
  
  return(sim_val)
  
}


needleman_wunsch <- function(s1, s2) {
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
  sim_val <- .jcall(sim_obj,"D", "needlemanWunch", s1, s2)
  
  return(sim_val)
  
}


smith_waterman <- function(s1, s2) {
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
  sim_val <- .jcall(sim_obj,"D", "smithWaterman", s1, s2)
  
  return(sim_val)
  
}

smith_waterman_gotoh <- function(s1, s2) {
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
  sim_val <- .jcall(sim_obj,"D", "smithWatermanGotoh", s1, s2)
  
  return(sim_val)
  
}











# numeric similarities
exact_match <- function(d1, d2) {
  if(is.null(s1) | is.null(s2)) {
    return(0) 
  }
  if(length(d1) == 0 | length(d2) == 0)
    return(0)
  if(d1 == d2)
    return(1)
  else
    return(0)
}

abs_diff <- function(d1, d2) {
  if(length(d1) > 0) {
    d1 <- as.numeric(d1)
    if(is.na(d1)) {
      return(NA)
    }
  }
  
  if(length(d2) > 0) {
    d2 <- as.numeric(d2)
    if(is.na(d2)) {
      return(NA)
    }
  }
  if(is.null(d1) | is.null(d2)) {
    return(0)
  }
  
  return(abs(d1 - d2))
}

rel_diff <- function(d1, d2) {
  if(length(d1) > 0) {
    d1 <- as.numeric(d1)
    if(is.na(d1)) {
      return(NA)
    }
  }
  
  if(length(d2) > 0) {
    d2 <- as.numeric(d2)
    if(is.na(d2)) {
      return(NA)
    }
  }
  if(is.null(d1) | is.null(d2)) {
    return(0)
  }
  return(abs(d1 - d2)/(d1+d2))
}

