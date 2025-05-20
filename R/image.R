#' getImageInfo
#'
#' get information of an ImgArray object
#'
#' @param object an ImgArray object
#' 
#' @export
#' @returns a data frame of width and height info
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
  if(length(dim_image) == 2){
    imginfo <- list(width = dim_image[1], height = dim_image[2])
  } else {
    imginfo <- list(width = dim_image[2], height = dim_image[3])
  }
  as.data.frame(imginfo)
}

#' read_image
#' 
#' @param image the image
#' @param engine the package to use for each image layer: either
#' \code{ebimage} or \code{magick}
#' 
#' @importFrom magick image_read 
#' @importFrom EBImage readImage
#' 
#' @noRd
#' @keywords internal
read_image <- function(image, engine){
  # if(engine == "magick-image"){
  #   image <- magick::image_read(image)
  # } else {
  #   image <- EBImage::readImage(image)
  # }
  switch(engine,
         `magick-image` = magick::image_read(image),
         `EBImage` = EBImage::readImage(image)
  )
}