#' dim
#' 
#' @param x An object of ImgArray class
#' @export
setMethod("dim", "ImgArray", function(x) dim(x[[1]]))

#' length
#'
#' @param x An object of ImgArray class
#' @export
setMethod("length", signature = "ImgArray", function(x) length(x@series))

#' path of ImgArray image
#'
#' @param object an ImgArray object
#' @importFrom DelayedArray path
#' @export
setMethod("path", 
          signature = "ImgArray",
          function(object){
            DelayedArray::path(object[[1]])
          }
)

#' path of ImgArray image
#'
#' @param object an ImgArray object
#' @param value the new path
#' @export
setReplaceMethod("path", 
                 signature = "ImgArray",
                 function(object, value){
                   n.series <- length(object)
                   for(i in 1:n.series){
                     path(object[[i]]@seed) <- value
                   }
                   return(object)
                 }
)

.isTRUEorFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

.isSingleString <- function (x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}
