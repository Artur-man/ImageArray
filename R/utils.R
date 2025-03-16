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
                   for(i in seq_len(n.series)){
                     object[[i]] <- 
                       modify_seeds(object[[i]],
                                    function(x) {
                                      path.name <- slotNames(x)[grepl("path", slotNames(x))]
                                      file_path <- slot(x, name = path.name)
                                      if(grepl(".zarr", file_path)){
                                        name <- strsplit(file_path, split = "\\.zarr")[[1]][2]
                                        value <- file.path(value, name)
                                      } 
                                      slot(x, name = path.name) <- value
                                      x
                                    })
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

#' modify_seeds
#'
#' @noRd
.modify_seeds <- function (x, FUN, ...) 
{
  if (is(x, "DelayedUnaryOp")) {
    x@seed <- modify_seeds(x@seed, FUN, ...)
  }
  else if (is(x, "DelayedNaryOp")) {
    x@seeds <- lapply(x@seeds, modify_seeds, FUN, ...)
  }
  else {
    x <- FUN(x, ...)
  }
  return(x)
}

