#' getImageInfo
#'
#' get information of an ImgArray object
#'
#' @param object an ImgArray object
#'
#' @importFrom stats setNames
#' 
#' @export
#' @returns a data frame of width and height info
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
#' getImageInfo(imgarray)
#'
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.series = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' getImageInfo(imgarray)
#' 
getImageInfo <- function(object) {
  ax <- axes(object)
  dim_image <- stats::setNames(dim(object[[1]]),ax)
  imginfo <- list(width = dim_image["x"], height = dim_image["y"])
  as.data.frame(imginfo, row.names = NULL)
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
read_image <- function(image, engine) {
  switch(
    engine,
    `magick-image` = magick::image_read(image),
    `EBImage` = EBImage::readImage(image)
  )
}