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
#'
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
#' @noRd
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
  definition = function(x){
    cat(class(x = object), "Object \n")
    n.series <- len(x)
    for(i in 1:n.series){
      dim_image <- dim(x@series[[i]])
      dim_image <- paste(dim_image, collapse = ",")
      cat(paste0("Series ", i, " of size (", dim_image, ") \n"))
    }
  }
)

#' aperm
#'
#' aperm
#'
#' @rdname aperm
#' @aliases aperm
#' @method aperm Image_Array
#' @export
aperm.Image_Array <- function(object, perm){
  n.series <- len(object)
  for(i in 1:n.series){
    object[[i]] <- aperm(object[[i]], perm = perm)
  }
  object
}

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

#' rotate.Image_Array
#'
#' rotate Image_Array image
#'
#' @param object an Image_Array object
#' @param degrees value between 0 and 360 for how many degrees to rotate
#'
#' @export
rotate.Image_Array <- function(object, degrees){
  
  # aperm
  if(degrees %in% c(90, 270)){
    object <- aperm(object, perm = c(1,3,2)) 
  }
  
  # flop
  if(degrees %in% c(90,180)){
    object <- flop(object)
  }
  
  # flip
  if(degrees %in% c(180, 270)){
    object <- flip(object)
  }
  
  # return
  object
}

#' negate Image_Array image
#'
#' @param object an Image_Array object
#'
#' @export
negate.Image_Array <- function(object){
  
  n.series <- len(object)
  for(i in 1:n.series){
    object[[i]] <- 255 - object[[i]]
  }
  object
}

#' flip Image_Array image
#'
#' @param image an Image_Array object
#'
#' @export
flip.Image_Array <- function(object){
  n.series <- len(object)
  for(i in 1:n.series){
    img <- object[[i]]
    dim_img <- dim(img)
    object[[i]] <- img[ , , dim_img[3]:1, drop = FALSE]
  }
  object
}

#' flop Image_Array image
#'
#' @param image an Image_Array object
#'
#' @export
flop.Image_Array <- function(object){
  n.series <- len(object)
  for(i in 1:n.series){
    img <- object[[i]]
    dim_img <- dim(img)
    object[[i]] <- img[ , dim_img[2]:1, , drop = FALSE]
  }
  object
}

#' crop Image_Array image
#'
#' @param image an Image_Array object
#' @param ind index list
#'
#' @export
crop.Image_Array <- function(object, ind){
  
  # check ind
  if(!is.list(ind))
    stop("'ind' should be a list of integers")
  if((length(dim(object[[1]])) - 1) != length(ind))
    stop("'ind' should be a list of integers")
  
  # crop all images
  n.series <- len(object)
  for(i in 1:n.series){
    img <- object[[i]]
    cur_ind <- lapply(ind, function(curind){
      seq(floor(head(curind,1)/(2^(i-1))), ceiling(tail(curind,1)/(2^(i-1))))
    })
    object[[i]] <- img[, cur_ind[[1]], cur_ind[[2]], drop = FALSE]
  }
  
  object
}

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
  cat(paste0("Creating Series ", 1, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
  image_list <- list(magick::image_data(image, channels = "rgb"))
  if(n.series > 1){
    cur_image <- image
    for(i in 2:n.series){
      dim_image <- ceiling(dim_image/2)
      cat(paste0("Creating Series ", i, " of size (", dim_image[1], ",", dim_image[2], ") \n"))
      cur_image <- magick::image_resize(cur_image, 
                                        geometry = magick::geometry_size_percent(50), 
                                        filter = "Gaussian")
      image_list[[i]] <- magick::image_data(cur_image, channels = "rgb")
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
#' @param format on disk fornat
#' @param replace Should the existing file be removed or not
#' @param n.series the number of series in the Image_Array
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
                            output = "my_image",
                            name = "",
                            format = c("HDF5ImageArray", "ZarrImageArray"), 
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
  
  # open ondisk store
  switch(format,
         HDF5ImageArray = {
           rhdf5::h5createFile(ondisk_path)
           rhdf5::h5createGroup(ondisk_path, group = name)
         }, 
         ZarrImageArray = {
           zarr.array <- pizzarr::zarr_open(store = ondisk_path)
           zarr.array$create_group(name)
         })
  
  # write all series
  for(i in 1:len(image_list)){
    img <- aperm(as.integer(image_list[[i]]), c(3,2,1))
    
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

#' filepath of Image_Array image
#'
#' @param image an Image_Array object
#'
#' @importFrom DelayedArray path
#'  
#' @export
filepath.Image_Array <- function(object){
  DelayedArray::path(object[[1]])
}

#' filepath of Image_Array image
#'
#' @param image an Image_Array object
#' 
#' @importFrom DelayedArray path
#' 
#' @export
"filepath<-.Image_Array" <- function(object, value){
  n.series <- len(object)
  for(i in 1:n.series){
    object[[i]]@seed@filepath <- value
  }
  return(object)
}

#' as.array method for ImageArray object
#' 
#' @rdname as.array
#' @aliases as.array
#' @method as.array Image_Array
#' 
#' @export
as.array.Image_Array <- function(object, max.pixel.size = NULL){
  if(is.null(max.pixel.size)){
    return(as.array(object[[1]]))
  } else {
    if(max.pixel.size %% 1 == 0){
      n.series = len(object)
      for(i in 1:n.series){
        dim_img <- dim(object[[i]])
        if(max.pixel.size >= min(dim_img[2:3])){
          return(as.array(object[[i]]))
        }
      }
    } else {
      stop("'max.pixel.size' should be an integer!")
    }
  }
}