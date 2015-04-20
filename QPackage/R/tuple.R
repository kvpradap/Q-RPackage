tuple <- function(...) {
 new ("tuple", list(...))
}

setClass("tuple", contains = "list")

