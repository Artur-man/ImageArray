#' @describeIn ImageArray-methods path of an ImageArray object
#' @param object an ImageArray object
#' @importFrom DelayedArray path
#' @export
setMethod("path", signature = "ImageArray", function(object) {
  
  # check DelayedArray seed
  obj <- object[[1]]
  if(!inherits(obj, "DelayedArray"))
    stop("The path method is only applicable to ImageArray objects ", 
         "whose layers with DelayedArray seeds.")
  file_path <- DelayedArray::path(obj)
  
  # check if path is a zarr path
  if (.zarr_path_exists(file_path) || grepl(".zarr", file_path)) {
    file_path <- strsplit(file_path, split = "\\/")[[1]]
    file_path <- normalizePath(
      paste(file_path[-length(file_path)], collapse = "/")
    )
    .collapse_slashes(file_path)
  } else {
    file_path
  }
})

#' @describeIn ImageArray-methods replace method for path(ImageArray)
#' @param object an ImageArray object
#' @param value the new path
#' @importFrom methods slotNames slot slot<-
#' @importFrom DelayedArray modify_seeds
#' @export
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
            if (.zarr_path_exists(file_path)) {
              if(!grepl(".zarr", file_path) || !grepl(".zarr", file_path))
                stop(
                  "The path of the ImageArray object or the ",
                  "replacement should be a .zarr extension ", 
                  "for replacement to take place!"
                )
              name <- strsplit(file_path, split = "\\.zarr")[[1]][2]
              value <- file.path(value, name)
              value <- .collapse_slashes(value)
            }
            methods::slot(x, name = path.name) <- value
            x
          }
        )
    }
    object
  }
)

#' @noRd
.normalizePathSeparators <- function(path) {
  path <- .collapse_slashes(path)
  # if (!startsWith(path, "/")) 
  #   path <- paste0("/", path)
  # if (endslash && !endsWith(path, "/"))
  #   path <- paste0(path, "/")
  path
}

#' @noRd
.collapse_slashes <- function(x) {
  gsub("/+", "/", x)
}

#' @noRd
.isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' is.sequential
#' @noRd
is.sequential <- function(x) {
  all(abs(diff(x)) == 1)
}


