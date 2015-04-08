#' Constructor function for QTable
#' Extends data.frame to contain meta data such as Id attribute
QTable <- function(..., idCol="") {
  new ("QTable", data.frame(...), idCol=idCol)
}

#' Class definition for QTable
#' Has two attributes: idField and genId
#' idField : column id name in the table
#' genId : flag to denote whether Id attribute must be generated.
setClass(
  "QTable",
  slots = c(idCol="character"
  ),
  contains="data.frame"
)

#' Generic function for CheckId
setGeneric("CheckId", function(object, idCol) standardGeneric("CheckId"))

#' Generic function for SetId
setGeneric("SetId", function(object, idCol) standardGeneric("SetId"))

setMethod("CheckId", representation(object="QTable", idCol="character"), function(object, idCol) {
  
  if(any(colnames(object) == idCol) == FALSE) {
    stop(c("Input data does not have ", idCol, " column"))
  }
  uniqValues <- unique(object[idCol])
  if(nrow(uniqValues) != nrow(object[idCol])) {
    return(FALSE)
  } else {
    return(TRUE)
  }
  #c("Inside Check Id ", idCol)
})


#' Method definition for CheckId from QTable
#' @param object: QTable to which id attribute should be added
#' @param idCol: column name from QTable
#' @return QTable: updated QTable
setMethod("SetId", representation(object="QTable", idCol="character"), function(object, idCol) {
  if(any(colnames(dat) == idCol) == FALSE) {
    stop(c("Input data does not have ", idCol, " column"))
  }
  if(CheckId(object, idCol) == TRUE) {
    object@idCol=idCol
    object@genId = FALSE
    message(c(idCol, " column was unique and made as primary key"))
    return(object)
  } else {
    message(c(idCol, " column was not unique"))
    return(object)
  }
  
})


setMethod("CheckId", representation(object="data.frame", idCol="character"), function(object, idCol) {"Not defined for data.frame"})

setMethod("SetId", representation(object="data.frame", idCol="character"), function(object, idCol) {"Not defined for data.frame"})