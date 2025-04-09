#' path of ImgArray image
#'
#' @param object an ImgArray object
#' @importFrom DelayedArray path
#' @export
#' @returns the path to ImgArray object store
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
#' @importFrom methods slotNames slot slot<-
#' @export
#' @return does not return a value, updates the path of the ImgArray object
setReplaceMethod(
  "path", 
  signature = "ImgArray",
  function(object, value){
    n.series <- length(object)
    for(i in seq_len(n.series)){
      object[[i]] <- 
        modify_seeds(
          object[[i]],
          function(x) {
            ind <- grepl("path", slotNames(x))
            path.name <- methods::slotNames(x)[ind]
            file_path <- methods::slot(x, name = path.name)
            if(grepl(".zarr", file_path)){
              name <- strsplit(file_path, 
                               split = "\\.zarr")[[1]][2]
              value <- file.path(value, name)
            } 
            methods::slot(x, name = path.name) <- value
            x
          })
    }
    return(object)
  }
)

#' @noRd
.isTRUEorFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
.isSingleString <- function (x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' modify_seeds
#' @importFrom methods is
#' @noRd
.modify_seeds <- function (x, FUN, ...) 
{
  if (methods::is(x, "DelayedUnaryOp")) {
    x@seed <- modify_seeds(x@seed, FUN, ...)
  }
  else if (methods::is(x, "DelayedNaryOp")) {
    x@seeds <- lapply(x@seeds, modify_seeds, FUN, ...)
  }
  else {
    x <- FUN(x, ...)
  }
  return(x)
}

#' is.sequential
#' @noRd
is.sequential <- function(x){
  all(abs(diff(x)) == 1)
}  

