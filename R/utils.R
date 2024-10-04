isTRUEorFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

isSingleString <- function (x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' filepath of Image_Array image
#'
#' @param image an Image_Array object
#'
#' @importFrom DelayedArray path
#'  
#' @export
filepath.Image_Array <- function(object){
  DelayedArray::path(object[[1]])
}

#' filepath of Image_Array image
#'
#' @param image an Image_Array object
#' 
#' @importFrom DelayedArray path
#' 
#' @export
"filepath<-.Image_Array" <- function(object, value){
  n.series <- len(object)
  for(i in 1:n.series){
    object[[i]]@seed@filepath <- value
  }
  return(object)
}