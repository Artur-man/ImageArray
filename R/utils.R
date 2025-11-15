#' path of ImageArray image
#'
#' @param object an ImageArray object
#' @importFrom DelayedArray path
#' @export
#' @returns the path to ImageArray object store
setMethod("path", signature = "ImageArray", function(object) {
  DelayedArray::path(object[[1]])
})

#' path of ImageArray image
#'
#' @param object an ImageArray object
#' @param value the new path
#' @importFrom methods slotNames slot slot<-
#' @export
#' @return does not return a value, updates the path of the ImageArray object
setReplaceMethod(
  "path",
  signature = "ImageArray",
  function(object, value) {
    n.levels <- length(object)
    for (i in seq_len(n.levels)) {
      object[[i]] <-
        modify_seeds(
          object[[i]],
          function(x) {
            ind <- grepl("path", slotNames(x))
            path.name <- methods::slotNames(x)[ind]
            file_path <- methods::slot(x, name = path.name)
            if (grepl(".zarr", file_path)) {
              name <- strsplit(file_path, split = "\\.zarr")[[1]][2]
              value <- file.path(value, name)
            }
            methods::slot(x, name = path.name) <- value
            x
          }
        )
    }
    return(object)
  }
)

#' @noRd
.isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' is.sequential
#' @noRd
is.sequential <- function(x) {
  all(abs(diff(x)) == 1)
}
