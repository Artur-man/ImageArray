####
# Objects and Classes ####
####

#' The Image_Array Class
#'
#' @slot series a list of DelayedArray
#'
#' @name Image_Array-class
#' @rdname Image_Array-class
#' @exportClass Image_Array
setClass(
  Class="Image_Array",
  slots=c(
    series="list"
  )
)

####
# Methods ####
####

#' len
#'
#' @param object An object of Image_Array class
#' 
#' @export
len <- function(object){
  return(length(object@series))
}

#' @importFrom methods slot
#' @noRd
setMethod(
  f = '[[',
  signature = 'Image_Array',
  definition = function(x, i){
    return(x@series[[i]])
  }
)

#' @importFrom methods slot
#' @noRd
setMethod(
  f = '[[<-',
  signature = c('Image_Array'),
  definition = function(x, i, ..., value){
    x@series[[i]] <- value
    return(x)
  }
)

#' @importFrom methods slot
#' @noRd
setMethod(
  f = 'show',
  signature = c('Image_Array'),
  definition = function(object){
    cat(class(x = object), "Object \n")
    n.series <- len(object)
    for(i in 1:n.series){
      dim_image <- dim(object@series[[i]])
      dim_image <- paste(dim_image, collapse = ",")
      cat(paste0("Series ", i, " of size (", dim_image, ") \n"))
    }
  }
)

#' createImageArray
#'
#' creates an object of ImageArray class
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
createImageArray <- function(image, n.series = NULL, verbose = FALSE)
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
  methods::new("Image_Array", series = image_list)
}

#' writeImageArray
#' 
#' Writing image arrays on disk
#'
#' @param image image
#' @param output output file name
#' @param name name of the group
#' @param format on disk format
#' @param replace Should the existing file be removed or not
#' @param n.series the number of series in the Image_Array
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
writeImageArray <- function(image, 
                            output = "my_image",
                            name = "",
                            format = c("InMemoryImageArray", "HDF5ImageArray", "ZarrImageArray"), 
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
  ondisk_path <- paste0(output, ifelse(format == "HDF5ImageArray", ".h5", ".zarr"))
  
  # create or replace output folder
  if (!isTRUEorFALSE(replace)) 
    stop("'replace' must be TRUE or FALSE")
  if(replace){
    if(file.exists(ondisk_path))
      file.remove(ondisk_path) 
  }
  
  # make Image Array
  if(!inherits(image, "Image_Array")){
    image_list <- createImageArray(image, n.series = n.series, verbose = verbose)
  } else {
    image_list <- image
  }

  # open ondisk store
  switch(format,
         HDF5ImageArray = {
           if(!file.exists(ondisk_path))
            rhdf5::h5createFile(ondisk_path)
           rhdf5::h5createGroup(ondisk_path, group = name)
         }, 
         ZarrImageArray = {
           if(!dir.exists(ondisk_path))
            zarr.array <- pizzarr::zarr_open(store = ondisk_path)
           zarr.array$create_group(name)
         })
  
  # write all series
  for(i in 1:len(image_list)){
    img <- array(as.integer(image_list[[i]]), dim = dim(image_list[[i]]))

    # write array
    switch(format,
           HDF5ImageArray = {
             image_list[[i]] <- HDF5Array::writeHDF5Array(img, filepath = ondisk_path, name = paste0(name,"/",i), 
                                                          chunkdim = chunkdim, 
                                                          level = level, as.sparse = as.sparse, 
                                                          with.dimnames = FALSE,verbose = verbose)
           }, 
           ZarrImageArray = {
             image_list[[i]] <- ZarrArray::writeZarrArray(img, filepath = ondisk_path, name = paste0(name, "/",i), 
                                                          chunkdim = chunkdim, 
                                                          level = level, as.sparse = as.sparse, 
                                                          with.dimnames = FALSE,verbose = verbose)
           }, 
           InMemoryImageArray = {
             image_list[[i]] <- img
           })
  }
  
  # return
  return(image_list)
}