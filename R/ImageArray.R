####
# Objects and Classes ####
####

#' The Image.Array Class
#'
#' @slot series a list of DelayedArray
#'
#' @name Image.Array-class
#' @rdname Image.Array-class
#' @exportClass Image.Array
#'
setClass(
  Class="Image.Array",
  slots=c(
    series="list"
  )
)

####
# Methods ####
####

#' len
#'
#' @param object An object of Image.Array class
#' 
#' @noRd
len <- function(object){
  return(length(object@series))
}

#' @importFrom methods slot
#' @noRd
setMethod(
  f = '[[',
  signature = 'Image.Array',
  definition = function(x, i){
    return(x@series[[i]])
  }
)

#' @importFrom methods slot
#' @noRd
setMethod(
  f = '[[<-',
  signature = c('Image.Array'),
  definition = function(x, i, ..., value){
    x@series[[i]] <- value
    return(x)
  }
)

#' @importFrom methods slot
#' @noRd
setMethod(
  f = 'show',
  signature = c('Image.Array'),
  definition = function(x){
    cat(class(x = object), "Object \n")
    n.series <- len(x)
    for(i in 1:n.series){
      dim_image <- dim(x@series[[i]])
      cat(paste0("Series ", i, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
    }
  }
)

####
# Functions ####
####

#' createImageArray
#'
#' creates an object of ImageArray class
#' 
#' @param image the image
#' @param n.series the number of series if the image supposed to be pyrimadil
#'
#' @importFrom magick image_read image_info image_resize image_data geometry_size_percent
#' 
#' @export
createImageArray <- function(image, n.series = NULL)
{
  if(!inherits(image, "magick-image")){
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
    # how many series of power of 2 required to get a maximum pixel size of 700 on either width or height
    n.series <- ceiling(log2(image_maxsize/700)) + 1
  } else if(n.series < 1){
    stop("'n.series' has to be 1 or a larger integer value!")
  }
  
  # create image series
  cat(paste0("Creating Series ", 1, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
  image_list <- list(magick::image_data(image, channels = "rgb"))
  if(n.series > 1){
    cur_image <- image
    for(i in 2:n.series){
      dim_image <- ceiling(dim_image/2)
      cat(paste0("Creating Series ", i, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
      cur_image <- magick::image_resize(cur_image, geometry = magick::geometry_size_percent(50))
      image_list[[i]] <- magick::image_data(cur_image, channels = "rgb")
    }
  }
  
  # return
  methods::new("Image.Array", series = image_list)
}

#' writeImageArray
#' 
#' Writing image arrays on disk
#'
#' @param image image
#' @param format on disk fornat
#' @param output output file name
#' @param replace Should the existing file be removed or not
#' @param n.series the number of series in the Image.Array
#' @param chunkdim chunkdim
#' @param level level
#' @param as.sparse as.sparse 
#' @param verbose 
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom ZarrArray writeZarrArray
#' 
#' @export
  writeImageArray <- function(image, 
                            format = c("HDF5ImageArray", "ZarrImageArray"), 
                            output = "my_image", 
                            replace = FALSE, 
                            n.series = NULL,
                            chunkdim=NULL, 
                            level=NULL,
                            as.sparse=NA,
                            verbose=NA)
{
  # check arguements
  if (!(is.logical(as.sparse) && length(as.sparse) == 1L))
    stop(wmsg("'as.sparse' must be NA, TRUE or FALSE"))
  verbose <- DelayedArray:::normarg_verbose(verbose)
  
  # path
  ondisk_path <- paste0(output, ifelse(format == "HDF5ImageArray", ".h5", ".zarr"))
  
  # create or replace output folder
  if (!isTRUEorFALSE(replace)) 
    stop("'replace' must be TRUE or FALSE")
  if(replace){
    if(file.exists(ondisk_path))
      file.remove(ondisk_path) 
  }
  
  # make Image Array
  image_list <- createImageArray(image, n.series = n.series)
  
  # write all series
  for(i in 1:len(image_list)){
    img <- aperm(as.integer(image_list[[i]]), c(3,2,1))
    
    switch(format,
           HDF5ImageArray = {
             image_list[[i]]  <-  HDF5Array::writeHDF5Array(img, filepath = paste0(output, ".h5"), name = paste(i), 
                                                            chunkdim = chunkdim, 
                                                            level = level, as.sparse = as.sparse, 
                                                            with.dimnames = FALSE,verbose = verbose)
           }, 
           ZarrImageArray = {
             image_list[[i]] <- ZarrArray::writeZarrArray(img, filepath = paste0(output, ".zarr"), name = paste(i), 
                                                          chunkdim = chunkdim, 
                                                          level = level, as.sparse = as.sparse, 
                                                          with.dimnames = FALSE,verbose = verbose)
           })
  }
  
  # return
  return(image_list)
}

####
# Auxiliary ####
####

isTRUEorFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

isSingleString <- function (x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}


