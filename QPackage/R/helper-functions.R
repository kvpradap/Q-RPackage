is_qtable <- function(table_obj) {
  # get the class
  cls <- class(table_obj)
  cls <- cls[1]
  if(cls == "qtable")
    return(TRUE)
  else
    return(FALSE)
}

# function to convert types of columns
convert_types <- function(df,classes){
  out <- lapply(1:length(classes),
                FUN = function(classIndex){as(df[,classIndex],classes[classIndex])})
  names(out) <- colnames(df)
  return(data.frame(out, stringsAsFactors = FALSE))
}


# check if a table is a labeled data set
is_labeled_table <- function(table_obj) {
  
  # check if the last column name is label
  num_cols <- length(colnames(table_obj))
  if(colnames(table_obj)[num_cols] != "label") {
    return(FALSE)
  }
  if( any(table_obj[, "label"] < 0 )  || any(table_obj[, "label"] > 1)) {
    return(FALSE)
  }
return(TRUE)
}

# check if its integer(0) 
is_integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}


# helper function to join candidate set with base tables. 
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

# display sim functions
show_builtin_simfuns <- function() {
  QPackage:::gbl_sim_fns...
}

show_builtin_tokenizers <- function() {
  QPackage:::gbl_toks...
}
