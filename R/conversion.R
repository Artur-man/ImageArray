#' as.array method for ImageArray object
#' 
#' @param max.pixel.size maximum pixel size 
#' @param min.pixel.size minimum pixel size
#' 
#' @rdname as.array
#' @aliases as.array
#' @method as.array Image_Array
#' 
#' @export
as.array.Image_Array <- function(object, max.pixel.size = NULL, min.pixel.size = NULL){
  
  # get parameter
  if(!is.null(max.pixel.size) && !is.null(min.pixel.size)){
    stop("min and max values cant be defined in the same time!")
  }
  
  if(is.null(max.pixel.size) && is.null(min.pixel.size)){
    return(as.array(object[[1]]))
  } else if(!is.null(max.pixel.size)){
    if(max.pixel.size %% 1 == 0){
      n.series = len(object)
      for(i in 1:n.series){
        dim_img <- dim(object[[i]])
        if(max.pixel.size >= max(dim_img[2:3])){
          return(as.array(object[[i]]))
        }
      }
    } else {
      stop("'max.pixel.size' should be an integer!")
    }
  } else if(!is.null(min.pixel.size)){
    if(min.pixel.size %% 1 == 0){
      n.series = len(object)
      if(n.series > 1){
        for(i in 2:n.series){
          dim_img <- dim(object[[i]])
          if(min.pixel.size > max(dim_img[2:3])){
            return(as.array(object[[i-1]]))
          }
        }
        # if no min check was attained, return the last image
        return(as.array(object[[i]]))
      } else {
        return(as.array(object[[1]]))
      }
    } else {
      stop("'max.pixel.size' should be an integer!")
    }
  }
}

#' as.raster method for ImageArray object
#' 
#' @param max.pixel.size maximum pixel size 
#' @param min.pixel.size minimum pixel size
#' 
#' @rdname as.raster
#' @aliases as.raster
#' @method as.raster Image_Array
#' 
#' @export
as.raster.Image_Array <- function(object, max.pixel.size = NULL, min.pixel.size = NULL){
  object <- as.array(object, max.pixel.size = max.pixel.size)
  object <- as_raster_array(aperm(object, perm = c(3,2,1)), max = 255)
}

as_raster_array <- function (x, max = 1, ...) 
{
  if (!is.numeric(x)) {
    if (is.raw(x)) {
      storage.mode(x) <- "integer"
      max <- 255L
    }
    else stop("a raster array must be numeric")
  }
  if (length(d <- dim(x)) != 3L) 
    stop("a raster array must have exactly 3 dimensions")
  r <- array(if (d[3L] == 3L) 
    rgb(t(x[, , 1L]), t(x[, , 2L]), t(x[, , 3L]), maxColorValue = max)
    else if (d[3L] == 4L) 
      rgb(t(x[, , 1L]), t(x[, , 2L]), t(x[, , 3L]), t(x[, , 4L]), maxColorValue = max)
    else if (d[3L] == 1L) 
      rgb(t(x[, , 1L]), t(x[, , 1L]), t(x[, , 1L]), maxColorValue = max)
    else stop("a raster array must have exactly 1, 3 or 4 planes"), 
    dim = d[1:2])
  class(r) <- "raster"
  r
}