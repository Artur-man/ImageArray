#' getImageInfo
#'
#' get information of an ImgArray object
#'
#' @param object an ImgArray object
#' @export
#' 
#' @examples
#' # get image
#' img.file <- system.file("extdata", "bird.png", package = "ImageArray")
#' 
#' # create ImgArray
#' dir.create(td <- tempfile())
#' output_h5ad <- file.path(td, "h5test")
#' imgarray <- writeImgArray(img.file, 
#'                           output = output_h5ad, 
#'                           name = "image",
#'                           format = "HDF5ImgArray", 
#'                           replace = TRUE, verbose = FALSE)
#' getImageInfo(imgarray)
#' 
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.series = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' getImageInfo(imgarray)
getImageInfo <- function(object){
  dim_image <- dim(object[[1]])
  imginfo <- list(width = dim_image[2], height = dim_image[3])
  as.data.frame(imginfo)
}