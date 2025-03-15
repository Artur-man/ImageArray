####
# Objects and Classes ####
####

#' The ImgArray Class
#'
#' @slot series a list of DelayedArray
#'
#' @name ImgArray-class
#' @rdname ImgArray-class
#' @exportClass ImgArray
setClass(
  Class="ImgArray",
  slots=c(
    series="list"
  )
)

####
# Methods ####
####

#' Methods for ImgArray
#'
#' Methods for \code{ImgArray} objects
#'
#' @param x An ImgArray object
#' @param i,value Depends on the usage
#' \describe{
#'  \item{\code{[[}, \code{[[<-}}{
#'    Here \code{i} is the level of the image pyramid. You can use the \code{length} function to get the number of the layers in the pyramid
#'  }
#' }
#' @param ... Arguments passed to other methods
#'
#' @name ImgArray-methods
#' @rdname ImgArray-methods
#'
#' @concept ImgArray
NULL

#' @describeIn ImgArray-methods Layer access for \code{ImgArray} objects
#' 
#' @importFrom methods slot
#' @export
setMethod(
  f = '[[',
  signature = c('ImgArray', "numeric"),
  definition = function(x, i){
    return(x@series[[i]])
  }
)

#' @describeIn ImgArray-methods Layer access for \code{ImgArray} objects
#' 
#' @importFrom methods slot
#' @export
setMethod(
  f = '[[<-',
  signature = c('ImgArray', "numeric"),
  definition = function(x, i, ..., value){
    x@series[[i]] <- value
    return(x)
  }
)

#' @importFrom methods slot
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
      cat(paste0("Series ", i, " of size (", dim_image, ") \n"))
    }
  }
)

#' createImgArray
#'
#' creates an object of ImgArray class
#' 
#' @param image the image
#' @param n.series the number of series if the image supposed to be pyrimadil
#' @param verbose verbose
#'
#' @importFrom magick image_read image_info image_resize image_data geometry_size_percent
#' @importFrom methods new
#' @importFrom DelayedArray DelayedArray
#' 
#' @export
createImgArray <- function(image, n.series = NULL, verbose = FALSE)
{
  # convert images
  if(is.integer(image)){
    image <- array(as.raw(image), dim = c(3,2,1))
  }
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
  if(verbose)
    cat(paste0("Creating Series ", 1, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
  image_data <- magick::image_data(image, channels = "rgb")
  image_list <- list(DelayedArray::DelayedArray(as.array(image_data)))
  if(n.series > 1){
    cur_image <- image
    for(i in 2:n.series){
      dim_image <- ceiling(dim_image/2)
      if(verbose)
        cat(paste0("Creating Series ", i, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
      cur_image <- magick::image_resize(cur_image, 
                                        geometry = magick::geometry_size_percent(50), 
                                        filter = "Gaussian")
      image_data <- magick::image_data(cur_image, channels = "rgb")
      image_list[[i]] <- DelayedArray::DelayedArray(as.array(image_data))
    }
  }
  
  # return
  methods::new("ImgArray", series = image_list)
}

#' writeImgArray
#' 
#' Writing image arrays on disk
#'
#' @param image image
#' @param output output file name
#' @param name name of the group
#' @param format on disk format
#' @param replace Should the existing file be removed or not
#' @param n.series the number of series in the ImgArray
#' @param chunkdim chunkdim
#' @param level level
#' @param as.sparse as.sparse 
#' @param verbose verbose
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom ZarrArray writeZarrArray
#' @import DelayedArray
#' 
#' @export
writeImgArray <- function(image, 
                            output = "my_image",
                            name = "",
                            format = c("InMemoryImgArray", "HDF5ImgArray", "ZarrImgArray"), 
                            replace = FALSE, 
                            n.series = NULL,
                            chunkdim=NULL, 
                            level=NULL,
                            as.sparse=NA,
                            verbose=FALSE)
{
  # check arguements
  if (!(is.logical(as.sparse) && length(as.sparse) == 1L))
    message("'as.sparse' must be NA, TRUE or FALSE")
  verbose <- DelayedArray:::normarg_verbose(verbose)
  
  # path
  ondisk_path <- paste0(output, ifelse(format == "HDF5ImgArray", ".h5", ".zarr"))
  
  # create or replace output folder
  if (!.isTRUEorFALSE(replace)) 
    stop("'replace' must be TRUE or FALSE")
  if(replace){
    if(file.exists(ondisk_path))
      file.remove(ondisk_path) 
  }
  
  # make Image Array
  if(!inherits(image, "ImgArray")){
    image_list <- createImgArray(image, n.series = n.series, verbose = verbose)
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
           zarr.array <- pizzarr::zarr_open(store = ondisk_path)
           zarr.array$create_group(name)
         })
  
  # write all series
  for(i in seq_len(length(image_list))){
    img <- array(as.integer(image_list[[i]]), dim = dim(image_list[[i]]))

    # write array
    switch(format,
           HDF5ImgArray = {
             image_list[[i]] <- HDF5Array::writeHDF5Array(img, filepath = ondisk_path, 
                                                          name = paste0(name,"/",i), 
                                                          chunkdim = chunkdim, 
                                                          level = level, as.sparse = as.sparse, 
                                                          with.dimnames = FALSE,verbose = verbose)
           }, 
           ZarrImgArray = {
             image_list[[i]] <- ZarrArray::writeZarrArray(img, filepath = ondisk_path, 
                                                          name = paste0(name, "/",i), 
                                                          chunkdim = chunkdim, 
                                                          level = level, as.sparse = as.sparse, 
                                                          with.dimnames = FALSE,verbose = verbose)
           }, 
           InMemoryImgArray = {
             image_list[[i]] <- img
           })
  }
  
  # return
  return(image_list)
}