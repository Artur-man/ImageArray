#' as.array
#'
#' as.array method for ImgArray object
#'
#' @param x an ImgArray object
#' @param max.pixel.size maximum pixel size
#' @param min.pixel.size minimum pixel size
#' @importFrom S4Arrays as.array.Array
#'
#' @export
#' @return An array object
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImgArray
#' dir.create(td <- tempfile())
#' output_h5ad <- file.path(td, "h5test")
#' imgarray <- writeImgArray(img.file,
#'                           output = output_h5ad,
#'                           name = "image",
#'                           format = "HDF5ImgArray",
#'                           replace = TRUE, verbose = FALSE)
#' imgarray <- realize(imgarray)
setMethod(
  "realize",
  signature = "ImgArray",
  function(x, max.pixel.size = NULL, min.pixel.size = NULL) {
    
    # axes
    ax <- axes(x)
    
    # get parameter
    if (!is.null(max.pixel.size) && !is.null(min.pixel.size)) {
      stop("min and max values cant be defined in the same time!")
    }

    if (is.null(max.pixel.size) && is.null(min.pixel.size)) {
      return(S4Arrays::as.array.Array(x[[1]]))
    } else if (!is.null(max.pixel.size)) {
      if (max.pixel.size %% 1 == 0) {
        n.levels <- length(x@levels)
        for (i in seq_len(n.levels)) {
          dim_img <- stats::setNames(dim(x[[i]]), ax)
          if (max.pixel.size >= max(rev(dim_img)[c("x", "y")])) {
            return(S4Arrays::as.array.Array(x[[i]]))
          }
        }
        return(S4Arrays::as.array.Array(x[[i]]))
      } else {
        stop("'max.pixel.size' should be an integer!")
      }
    } else if (!is.null(min.pixel.size)) {
      if (min.pixel.size %% 1 == 0) {
        n.levels <- length(x@levels)
        if (n.levels > 1) {
          for (i in 2:n.levels) {
            dim_img <- stats::setNames(dim(x[[i]]), ax)
            if (min.pixel.size > max(rev(dim_img)[c("x", "y")])) {
              return(S4Arrays::as.array.Array(x[[i - 1]]))
            }
          }
          return(S4Arrays::as.array.Array(x[[i - 1]]))
          # if no min check was attained, return the last image
          return(S4Arrays::as.array.Array(x[[i]]))
        } else {
          return(S4Arrays::as.array.Array(x[[1]]))
        }
      } else {
        stop("'max.pixel.size' should be an integer!")
      }
    }
  }
)

#' .as_raster_array
#'
#' custom as_raster_array function for ImgArray
#'
#' @param x x
#' @param max max
#' @importFrom  grDevices rgb
#' @noRd
.as_raster_array <- function(x, max = 1) {
  if (!is.numeric(x)) {
    if (is.raw(x)) {
      storage.mode(x) <- "integer"
      max <- 255L
    } else {
      stop("a raster array must be numeric")
    }
  }
  if (length(d <- dim(x)) != 3L) {
    stop("a raster array must have exactly 3 dimensions")
  }
  r <- array(
    if (d[3L] == 3L) {
      grDevices::rgb(t(x[,, 1L]), t(x[,, 2L]), t(x[,, 3L]), maxColorValue = max)
    } else if (d[3L] == 4L) {
      grDevices::rgb(
        t(x[,, 1L]),
        t(x[,, 2L]),
        t(x[,, 3L]),
        t(x[,, 4L]),
        maxColorValue = max
      )
    } else if (d[3L] == 1L) {
      grDevices::rgb(t(x[,, 1L]), t(x[,, 1L]), t(x[,, 1L]), maxColorValue = max)
    } else {
      stop("a raster array must have exactly 1, 3 or 4 planes")
    },
    dim = d[seq_len(2)]
  )
  class(r) <- "raster"
  r
}

#' as.raster method for ImgArray object
#'
#' @param x an ImgArray object
#' @param max.pixel.size maximum pixel size
#' @param min.pixel.size minimum pixel size
#'
#' @importFrom stats setNames
#' 
#' @export
#' @return A raster array
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImgArray
#' dir.create(td <- tempfile())
#' output_h5ad <- file.path(td, "h5test")
#' imgarray <- writeImgArray(img.file,
#'                           output = output_h5ad,
#'                           name = "image",
#'                           format = "HDF5ImgArray",
#'                           replace = TRUE, verbose = FALSE)
#' imgarray_raster <- as.raster(imgarray)
setMethod(
  "as.raster",
  signature = "ImgArray",
  function(x, max.pixel.size = NULL, min.pixel.size = NULL) {
    
    # get axes
    ax <- axes(x)
    cur_perm <- stats::setNames(seq_len(length(dim(x))), ax)

    # realize
    rx <- realize(
      x,
      max.pixel.size = max.pixel.size,
      min.pixel.size = min.pixel.size
    )
    d <- length(dim(x))
    if (d == 2) {
      cur_perm <- stats::setNames(c(cur_perm, 3), c(ax, "c"))
      rx <- array(rx, dim = c(dim(rx), 1))
    }
    rx <- aperm(rx, perm = cur_perm[c("y", "x", "c")])
    rx <- .as_raster_array(
      rx,
      max = if (type(x) == "double") 1 else 255
    )
    rx
  }
)
