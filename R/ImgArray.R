####
# Methods ####
####

#' Methods for ImgArray
#'
#' Methods for \code{ImgArray} objects
#'
#' @param x An ImgArray object
#' @param i,j,value Depends on the usage
#' \describe{
#'  \item{\code{[[}, \code{[[<-}}{
#'    Here \code{i} is the level of the image pyramid. 
#'    You can use the \code{length} function to get the 
#'    number of the layers in the pyramid. 
#'    When used with \code{crop}, arguments \code{i} and \code{j} are 
#'    associated with indices of image dimensions (e.g. width, height)
#'  }
#' }
#' @param ... Arguments passed to other methods
#'
#' @name ImgArray-methods
#' @rdname ImgArray-methods
#' 
#' @aliases 
#' [[,ImgArray-methods-method
#' [[<-,ImgArray-methods-method
#' 
#' @examples
#' # get image
#' img.file <- system.file("extdata", 
#' "bird.png", 
#' package = "ImageArray")
#' 
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.series = 3)
#' 
#' # access layers
#' imgarray[[1]]
#' imgarray[[2]]
#' 
#' # dimensions and length
#' dim(imgarray)
#' length(imgarray)
NULL

#' @describeIn ImgArray-methods subset and crop
#' for \code{ImgArray} objects
#' 
#' @export
setMethod(
  f = '[',
  signature = c('ImgArray', "numeric", "numeric"),
  definition = function(x, i, j, ...){
    crop(x, ind = list(i, j))
  }
)

#' @describeIn ImgArray-methods Layer access 
#' for \code{ImgArray} objects
#' 
#' @export
setMethod(
  f = '[[',
  signature = c('ImgArray', "numeric"),
  definition = function(x, i){
    return(x@series[[i]])
  }
)

#' @describeIn ImgArray-methods Layer access 
#' for \code{ImgArray} objects
#' 
#' @export
setMethod(
  f = '[[<-',
  signature = c('ImgArray', "numeric"),
  definition = function(x, i, ..., value){
    x@series[[i]] <- value
    return(x)
  }
)

#' @noRd
setMethod(
  f = 'show',
  signature = c('ImgArray'),
  definition = function(object){
    cat(class(x = object), "Object \n")
    n.series <- length(object)
    for(i in seq_len(n.series)){
      dim_image <- dim(object@series[[i]])
      dim_image <- paste(dim_image, collapse = ",")
      cat(paste0("Series ", i, 
                 " of size (", 
                 dim_image, ") \n"))
    }
  }
)

#' @describeIn ImgArray-methods dimensions of an ImgArray
#' @export
#' @returns dim of the first series of the ImgArray object
setMethod("dim", 
          "ImgArray", 
          function(x) dim(x[[1]]))

#' @describeIn ImgArray-methods dimensions of an ImgArray
#' @export
#' @returns type of ImgArray object
setMethod("type", 
          "ImgArray", 
          function(x) type(x[[1]]))

#' @describeIn ImgArray-methods length of an ImgArray
#' @export
#' @returns length of ImgArray object
setMethod("length", 
          signature = "ImgArray", 
          function(x) length(x@series))

#' @describeIn ImgArray-methods ImgArray constructor method
#'
#' A function for creating objects of ImgArray class
#' 
#' @param series the series IDs of the pyramidal image, 
#' typical an integer starting from 1
#'
#' @importFrom S4Vectors new2
#' @export
#' @return An ImgArray object
ImgArray <- function(series){
  S4Vectors::new2("ImgArray",
                  series = series)
}

#' createBFArray
#'
#' creates an object of BFArray class
#' 
#' @param image the image
#' @param series the series IDs of the pyramidal image, 
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the pyramidal 
#' image, typical an integer starting from 1
#' @param verbose verbose
#' 
#' @noRd
createBFArray <- function(image, 
                          series = NULL, 
                          resolution = NULL, 
                          verbose = FALSE){
  
  # check for nulls
  if(is.null(series)) series <- 1
  if(is.null(resolution)) resolution <- 1
  
  # make list
  image_list <- lapply(resolution, function(res){
    BFArray(image, series = series, resolution = res)
  })
  ImgArray(series = image_list)
}

#' createMagickArray
#'
#' creates an object of ImgArray class from magick image
#' 
#' @param image the image
#' @param n.series the number of series if the 
#' image supposed to be pyramidal
#' @param max.pixel.threshold the maximum width 
#' or height pixel length for output image 
#' @param verbose verbose
#' 
#' @noRd
createMagickArray <- function(image, 
                              n.series = NULL, 
                              max.pixel.threshold = 700, 
                              verbose = FALSE){
  
  if(inherits(image, "bitmap")){
    image <- magick::image_read(image)
  }
  
  # get image info
  image_info <- magick::image_info(image)
  dim_image <- c(image_info$width, image_info$height)
  
  # series
  if(is.null(n.series)){
    
    # get image size and resolution
    image_maxsize_id <- which.max(dim_image)
    image_maxsize <- dim_image[image_maxsize_id]
    
    # get number of series
    # how many series of power of 2 required to 
    # get a maximum pixel size of 700 on either width or height
    n.series <- ceiling(log2(image_maxsize/max.pixel.threshold)) + 1
  } else if(n.series < 1){
    stop("'n.series' has to be 1 or a larger integer value!")
  }
  
  # create image series
  if(verbose)
    cat(paste0("Creating Series ", 1, 
               " of size (", dim_image[1], 
               ",", dim_image[2], ") \n"))
  image_data <- magick::image_data(image, channels = "rgb")
  image_list <- list(DelayedArray::DelayedArray(as.array(image_data)))
  if(n.series > 1){
    cur_image <- image
    for(i in 2:n.series){
      dim_image <- ceiling(dim_image/2)
      if(verbose)
        cat(paste0("Creating Series ", i, 
                   " of size (", dim_image[1], 
                   ",", dim_image[2], ") \n"))
      cur_image <- magick::image_resize(
        cur_image, 
        geometry = magick::geometry_size_percent(50), 
        filter = "Gaussian")
      image_data <- magick::image_data(cur_image, 
                                       channels = "rgb")
      image_list[[i]] <- 
        DelayedArray::DelayedArray(as.array(image_data))
    }
  }
  
  # return
  ImgArray(series = image_list)
}

#' createImgArray
#'
#' creates an object of ImgArray class
#' 
#' @param image the image
#' @param n.series the number of series if the image supposed to be 
#' pyramidal, or the the series IDs of the pyramidal image, 
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the pyramidal image, 
#' typical an integer starting from 1
#' @param max.pixel.threshold the maximum width or height pixel 
#' length for output image 
#' @param verbose verbose
#'
#' @importFrom magick image_read 
#' @importFrom magick image_info 
#' @importFrom magick image_resize 
#' @importFrom magick image_data
#' @importFrom magick geometry_size_percent
#' @importFrom methods new
#' @importFrom DelayedArray DelayedArray
#' 
#' @export
#' @return An ImgArray object
#' 
#' @examples
#' # get image
#' img.file <- system.file("extdata", 
#' "bird.png", package = "ImageArray")
#' 
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.series = 3)
#' imgarray_raster <- as.raster(imgarray,
#'  max.pixel.size = 300)
#' plot(imgarray_raster)
#' 
createImgArray <- function(image, 
                           n.series = NULL, 
                           resolution = NULL, 
                           max.pixel.threshold = 700, 
                           verbose = FALSE)
{
  # convert to bitmap array if integer
  if(is.integer(image)){
    image <- array(as.raw(image), dim = c(3,2,1))
    image <- magick::image_read(image)
  }
  
  # create ImgArray from magick
  if(inherits(image, c("magick-image", "bitmap"))){
    return(createMagickArray(
      image, 
      n.series = n.series, 
      max.pixel.threshold = max.pixel.threshold, 
      verbose = verbose))
  }
  
  # check image format
  if(inherits(image, "character")){
    if(grepl(".ome.tiff$|.ome.tif$|.qptiff$|.qptif$", image)){
      createBFArray(image, 
                    series = n.series, 
                    resolution = resolution)
    } else {
      image <- magick::image_read(image)
      createMagickArray(image, 
                        n.series = n.series, 
                        max.pixel.threshold = max.pixel.threshold, 
                        verbose = verbose)
    }
  }
}

#' writeImgArray
#' 
#' Writing image arrays on disk
#'
#' @param image image
#' @param output output file name
#' @param name name of the group
#' @param format on disk format
#' @param replace Should the existing file be 
#' removed or not
#' @param n.series the number of series in the 
#' ImgArray
#' @param chunkdim The dimensions of the chunks 
#' to use for writing the data to disk.
#' @param level The compression level to use for 
#' writing the data to disk.
#' @param verbose verbose
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom Rarr writeZarrArray
#' @import DelayedArray
#' 
#' @export
#' @returns An ImgArray object
#'  
#' @examples
#' # get image
#' img.file <- system.file("extdata", 
#' "bird.png", package = "ImageArray")
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
#' plot(imgarray_raster)
#' 
writeImgArray <- function(image, 
                          output="my_image",
                          name="",
                          format=c("InMemoryImgArray", 
                                   "HDF5ImgArray", 
                                   "ZarrImgArray"), 
                          replace=FALSE, 
                          n.series=NULL,
                          chunkdim=NULL, 
                          level=NULL,
                          verbose=FALSE)
{
  # verbose
  verbose <- DelayedArray:::normarg_verbose(verbose)
  
  # path
  ondisk_path <- paste0(output, 
                        ifelse(format == "HDF5ImgArray", 
                               ".h5", ".zarr"))
  
  # create or replace output folder
  if (!.isTRUEorFALSE(replace)) 
    stop("'replace' must be TRUE or FALSE")
  if(replace){
    if(file.exists(ondisk_path))
      file.remove(ondisk_path) 
  }
  
  # make Image Array
  if(!inherits(image, "ImgArray")){
    image_list <- createImgArray(image, 
                                 n.series = n.series, 
                                 verbose = verbose)
  } else {
    image_list <- image
  }

  # open ondisk store
  switch(format,
         HDF5ImgArray = {
           if(!file.exists(ondisk_path))
            rhdf5::h5createFile(ondisk_path)
           rhdf5::h5createGroup(ondisk_path, group = name)
         }, 
         ZarrImgArray = {
           dir.zarr <- gsub(paste0(basename(ondisk_path), "$"), 
                            "", ondisk_path)
           open_zarr(dir = dir.zarr, 
                     name = basename(ondisk_path))
           zarrcreateGroup(ondisk_path, name)
         })
  
  # write all series
  for(i in seq_len(length(image_list))){
    img <- array(as.integer(image_list[[i]]), 
                 dim = dim(image_list[[i]]))

    # write array
    switch(format,
           HDF5ImgArray = {
             image_list[[i]] <- 
               HDF5Array::writeHDF5Array(
                 img, filepath = ondisk_path, 
                 name = paste0(name,"/",i), 
                 chunkdim = chunkdim, 
                 level = level, as.sparse = FALSE, 
                 with.dimnames = FALSE, 
                 verbose = verbose)
           }, 
           ZarrImgArray = {
             image_list[[i]] <- 
               Rarr::writeZarrArray(
                 img, 
                 zarr_array_path = 
                   file.path(ondisk_path, 
                             paste0(name, "/",i)), 
                 chunk_dim = c(dim(img)[1], 
                               min(dim(img)[2], 2000), 
                               min(dim(img)[3], 2000)))
           }, 
           InMemoryImgArray = {
             image_list[[i]] <- img
           })
  }
  
  # return
  return(image_list)
}