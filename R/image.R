#' getImageInfo
#'
#' get information on images
#'
#' @param image an Image_Array object
#'
#' @export
getImageInfo <- function(object){
  dim_image <- dim(object[[1]])
  imginfo <- list(width = dim_image[2], height = dim_image[3])
  as.data.frame(imginfo)
}