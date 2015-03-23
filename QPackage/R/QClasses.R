#' Constructor function for QTable
#' Extends data.frame to contain meta data such as Id attribute
QTable <- function(..., genId=FALSE) {
  new ("QTable", data.frame(...), genId=genId)
}

#' Class definition for QTable
#' Has two attributes: idField and genId
#' idField : column id name in the table
#' genId : flag to denote whether Id attribute must be generated.
setClass(
  "QTable",
  slots = c(idField="character",
            genId = "logical"
  ),
  contains="data.frame"
)

#' Generic function for CheckId
setGeneric("CheckId", function(object, idName) standardGeneric("CheckId"))

#' Generic function for AddId
setGeneric("AddId", function(object, idName) standardGeneric("AddId"))


setMethod("CheckId", representation(object="QTable", idName="character"), function(object, idName) {
  
  if(any(colnames(dat) == idName) == FALSE) {
    stop(c("Input data does not have ", idName, " column"))
  }
  uniqValues <- unique(object[idName])
  if(nrow(uniqValues) != nrow(object[idName])) {
    return(FALSE)
  } else {
    return(TRUE)
  }
  #c("Inside Check Id ", idName)
})


#' Method definition for AddId from QTable
#' @param object: QTable to which id attribute should be added
#' @param idName: column name from QTable
#' @return QTable: updated QTable
setMethod("AddId", representation(object="QTable", idName="character"), function(object, idName) {
  if(any(colnames(dat) == idName) == FALSE) {
    stop(c("Input data does not have ", idName, " column"))
  }
  if(CheckId(object, idName) == TRUE) {
    object@idField=idName
    object@genId = FALSE
    message(c(idName, " column was unique and made as primary key"))
    return(object)
  } else {
    message(c(idName, " column was not unique"))
    return(object)
  }
  
})


setMethod("CheckId", representation(object="data.frame", idName="character"), function(object, idName) {"Not defined for data.frame"})

setMethod("AddId", representation(object="data.frame", idName="character"), function(object, idName) {"Not defined for data.frame"})