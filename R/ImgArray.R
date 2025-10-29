####
# Methods ####
####

#' Methods for ImgArray
#'
#' Methods for \code{ImgArray} objects
#'
#' @param x,a,object An ImgArray object
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
#' @param drop ignored
#' @param degrees value between 0 and 360 for how many degrees to rotate
#' @param brightness the brightness of the new image in percentage, e.g. 120
#' @param perm perm
#' @param ind index list
#' @param ... Arguments passed to other methods
#'
#' @name ImgArray-methods
#' @rdname ImgArray-methods
#'
#' @aliases
#' [[,ImgArray,numeric-method
#' [[<-,ImgArray,numeric-method
#' rotate
#' rotate,ImgArray-method
#' crop
#' crop,ImgArray-method
#' flip
#' flip,ImgArray-method
#' flop
#' flop,ImgArray-method
#' negate
#' negate,ImgArray-method
#' modulate
#' modulate,ImgArray-method
#' axes
#' axes,ImgArray-method
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.levels = 3)
#'
#' # access layers
#' imgarray[[1]]
#' imgarray[[2]]
#'
#' # dimensions and length
#' dim(imgarray)
#' length(imgarray)
#'
#' # manipulate images
#' imgarray <- crop(imgarray, ind = list(100:200, 100:200))
#' imgarray <- rotate(imgarray, degrees = 90)
#' imgarray <- flip(imgarray)
#' imgarray <- flop(imgarray)
NULL

#' @describeIn ImgArray-methods subset and crop
#' for \code{ImgArray} objects
#'
#' @export
setMethod(
  f = '[',
  signature = c('ImgArray', "numeric", "numeric"),
  definition = function(x, i, j, ..., drop = FALSE) {
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
  definition = function(x, i) {
    return(x@levels[[i]])
  }
)

#' @describeIn ImgArray-methods Layer access
#' for \code{ImgArray} objects
#'
#' @export
setMethod(
  f = '[[<-',
  signature = c('ImgArray', "numeric"),
  definition = function(x, i, ..., value) {
    x@levels[[i]] <- value
    return(x)
  }
)

#' @noRd
setMethod(
  f = 'show',
  signature = c('ImgArray'),
  definition = function(object) {
    cat(class(x = object), "Object", 
        paste0(
          "(", paste(object@meta[["axes"]], collapse = ","), ")"
        ), 
    "\n")
    n.levels <- length(object@levels)
    for (i in seq_len(n.levels)) {
      dim_image <- dim(object@levels[[i]])
      dim_image <- paste(dim_image, collapse = ",")
      cat(paste0("Level ", i, " (", dim_image, ") \n"))
    }
  }
)

#' @describeIn ImgArray-methods dimensions of an ImgArray
#' @export
#' @returns dim of the first level of the ImgArray object
setMethod("dim", "ImgArray", function(x) dim(x[[1]]))

#' @describeIn ImgArray-methods dimensions of an ImgArray
#' @export
#' @returns type of ImgArray object
setMethod("type", "ImgArray", function(x) type(x[[1]]))

#' @describeIn ImgArray-methods length of an ImgArray
#' @export
#' @returns length of ImgArray object
setMethod("length", signature = "ImgArray", function(x) length(x@levels))

#' @describeIn ImgArray-methods ImgArray constructor method
#'
#' A function for creating objects of ImgArray class
#'
#' @param meta the metadata of the ImgArray object. 
#' @param levels levels of the pyramid image, typically a vector of integers
#' starting with 1
#'
#' @importFrom S4Vectors new2
#' @export
#' @return An ImgArray object
ImgArray <- function(meta, levels) {
  S4Vectors::new2("ImgArray", meta = meta, levels = levels)
}

#' createBFArray
#'
#' creates an object of BFArray class
#'
#' @param image the image
#' @param series the number of series if the image supposed to be
#' pyramidal, or the the series IDs of the pyramidal image,
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the pyramidal
#' image, typical an integer starting from 1
#' @param verbose verbose
#'
#' @noRd
createBFArray <- function(
  image,
  series = NULL,
  resolution = NULL,
  verbose = FALSE
) {
  # check for nulls
  if (is.null(series)) {
    series <- 1
  }
  if (is.null(resolution)) {
    resolution <- 1
  }

  # make list
  image_list <- lapply(resolution, function(res) {
    BFArray(image, series = series, resolution = res)
  })
  ImgArray(meta = list(axes = c("x", "y", "c")), levels = image_list)
}

#' createMagickArray
#'
#' creates an object of ImgArray class from magick image
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels. 
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param verbose verbose
#'
#' @importFrom magick image_read
#' @importFrom magick image_info
#' @importFrom magick image_resize
#' @importFrom magick image_data
#' @importFrom magick geometry_size_percent
#'
#' @noRd
createMagickArray <- function(
  image,
  n.levels = NULL,
  max.pixel.threshold = 700,
  verbose = FALSE
) {
  # check image
  if (inherits(image, "bitmap")) {
    image <- magick::image_read(image)
  }

  # get image info
  image_info <- magick::image_info(image)
  dim_image <- c(image_info$width, image_info$height)

  # levels
  if (is.null(n.levels)) {
    # get image size and resolution
    image_maxsize_id <- which.max(dim_image)
    image_maxsize <- dim_image[image_maxsize_id]

    # get number of levels
    # how many levels of power of 2 required to
    # get a maximum pixel size of 700 on either width or height
    n.levels <- ceiling(log2(image_maxsize / max.pixel.threshold)) + 1
  } else if (n.levels < 1) {
    stop("'n.levels' has to be 1 or a larger integer value!")
  }

  # create image levels
  if (verbose)
    .img_create_msg(dim(image), 1)
  image_data <- magick::image_data(image, channels = "rgb")
  storage.mode(image_data) <- "integer"
  image_list <- list(DelayedArray::DelayedArray(as.array(image_data)))
  if (n.levels > 1) {
    cur_image <- image
    for (i in 2:n.levels) {
      dim_image <- ceiling(dim_image / 2)
      if (verbose)
        .img_create_msg(dim_image, 1)
      cur_image <- magick::image_resize(
        cur_image,
        geometry = magick::geometry_size_percent(50),
        filter = "Gaussian"
      )
      image_data <- magick::image_data(cur_image, channels = "rgb")
      storage.mode(image_data) <- "integer"
      image_list[[i]] <-
        DelayedArray::DelayedArray(as.array(image_data))
    }
  }

  # return
  ImgArray(meta = list(axes = c("c", "x", "y")), levels = image_list)
}

#' createMagickArray
#'
#' creates an object of ImgArray class from magick image
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels. 
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param verbose verbose
#'
#' @importFrom EBImage readImage
#' @importFrom EBImage resize
#'
#' @noRd
createEBImageArray <- function(
  image,
  n.levels = NULL,
  max.pixel.threshold = 700,
  verbose = FALSE
) {
  # get and image info
  image_info <- dim(image)
  dim_image <- c(image_info[1], image_info[2])

  # levels
  if (is.null(n.levels)) {
    # get image size and resolution
    image_maxsize_id <- which.max(dim_image)
    image_maxsize <- dim_image[image_maxsize_id]

    # get number of levels
    # how many levels of power of 2 required to
    # get a maximum pixel size of 700 on either width or height
    n.levels <- ceiling(log2(image_maxsize / max.pixel.threshold)) + 1
  } else if (n.levels < 1) {
    stop("'n.levels' has to be 1 or a larger integer value!")
  }

  # create image levels
  meta <- list(axes = c("x", "y", "c"))
  if (verbose)
    .img_create_msg(dim_image, 1)
  img_perm <- if(length(dim(image)) == 2) c(1,2) else c(1, 2, 3)
  meta[["axes"]] <- meta[["axes"]][img_perm]
  img_perm <- stats::setNames(img_perm, meta[["axes"]])
  img <- aperm(image, img_perm)
  image_list <- list(DelayedArray::DelayedArray(img))
  if (n.levels > 1) {
    cur_image <- image
    for (i in 2:n.levels) {
      dim_image <- ceiling(dim_image / 2)
      if (verbose)
        .img_create_msg(dim_image, i)
      resize_factor <- dim_image
      cur_image <- EBImage::resize(
        cur_image,
        w = dim_image[1],
        h = dim_image[2]
      )
      cur_img <- aperm(cur_image, img_perm)
      image_list[[i]] <-
        DelayedArray::DelayedArray(cur_img)
    }
  }

  # return
  ImgArray(meta = meta, levels = image_list)
}

#' createImgArray
#'
#' creates an object of ImgArray class
#'
#' @param image the image
#' @param n.levels the number of levels of the pyramidal image,
#' typical an integer starting from 1
#' @param series the series IDs of the pyramidal image,
#' typical an integer starting from 1. 
#' @param resolution the resolution IDs of the pyramidal image,
#' typical an integer starting from 1. 
#' @param max.pixel.threshold the maximum width
#' and height pixel dimension that the lowest level of the image pyramid
#' should have, thus the image will be downscaled two folds until both width
#' and height is below the threshold. Default is 700 pixels. 
#' If \code{n.levels} is provided, this parameter will be ignored.
#' @param engine the package to use for each image layer: either
#' \code{EBImage} or \code{magick-image}
#' @param verbose verbose
#'
#' @importFrom methods new
#' @importFrom DelayedArray DelayedArray
#'
#' @export
#' @return An ImgArray object
#'
#' @examples
#' # get image
#' library(EBImage)
#' img.file <- system.file("images", "sample.png", package="EBImage")
#'
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.levels = 3)
#' imgarray_raster <- as.raster(imgarray, max.pixel.size = 300)
#' plot(imgarray_raster)
#'
createImgArray <- function(
  image,
  n.levels = NULL,
  series = NULL,
  resolution = NULL,
  max.pixel.threshold = 700,
  engine = "EBImage",
  verbose = FALSE
) {
  # convert to bitmap array if integer
  if (is.integer(image)) {
    if (engine == "magick-image") {
      image <- array(as.raw(image), dim = c(3, 2, 1))
    }
    image <- read_image(image, engine = engine)
  }

  # create ImgArray from magick
  if (inherits(image, c("magick-image", "bitmap"))) {
    return(createMagickArray(
      image,
      n.levels = n.levels,
      max.pixel.threshold = max.pixel.threshold,
      verbose = verbose
    ))
  }

  # create ImgArray from EBImage
  if (inherits(image, c("Image"))) {
    return(createEBImageArray(
      image,
      n.levels = n.levels,
      max.pixel.threshold = max.pixel.threshold,
      verbose = verbose
    ))
  }

  # check image format
  if (inherits(image, "character")) {
    if (grepl(".ome.tiff$|.ome.tif$|.qptiff$|.qptif$", image)) {
      image <- createBFArray(image, series = series, resolution = resolution)
    } else {
      image <- read_image(image, engine = engine)
      if (inherits(image, "magick-image")) {
        createMagickArray(
          image,
          n.levels = n.levels,
          max.pixel.threshold = max.pixel.threshold,
          verbose = verbose
        )
      } else if (inherits(image, "Image")) {
        createEBImageArray(
          image,
          n.levels = n.levels,
          max.pixel.threshold = max.pixel.threshold,
          verbose = verbose
        )
      }
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
#' @param n.levels the number of levels if the image supposed to be
#' pyramidal. 
#' @param chunkdim The dimensions of the chunks
#' to use for writing the data to disk.
#' @param level The compression level to use for
#' writing the data to disk.
#' @param engine the package to use for each image layer: either
#' \code{EBImage} or \code{magick-image}
#' @param verbose verbose
#' @param ... additional parameters passed to \link[ImageArray]{createImgArray}.
#'
#' @importFrom HDF5Array writeHDF5Array
#' @importFrom Rarr writeZarrArray
#' @importFrom rhdf5 h5createFile h5createGroup
#' @import DelayedArray
#'
#' @export
#' @returns An ImgArray object
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
#' imgarray_raster <- as.raster(imgarray)
#' plot(imgarray_raster)
#'
writeImgArray <- function(
  image,
  output = "my_image",
  name = "",
  format = c("InMemoryImgArray", "HDF5ImgArray", "ZarrImgArray"),
  replace = FALSE,
  n.levels = NULL,
  chunkdim = NULL,
  level = NULL,
  engine = "EBImage",
  verbose = FALSE,
  ...
) {
  # verbose
  verbose <- DelayedArray:::normarg_verbose(verbose)

  # path
  ondisk_path <- paste0(
    output,
    ifelse(format == "HDF5ImgArray", ".h5", ".zarr")
  )

  # create or replace output folder
  if (!.isTRUEorFALSE(replace)) {
    stop("'replace' must be TRUE or FALSE")
  }
  if (replace) {
    if (file.exists(ondisk_path)) {
      file.remove(ondisk_path)
    }
  }

  # make Image Array
  if (!inherits(image, "ImgArray")) {
    image_list <- createImgArray(
      image,
      n.levels = n.levels,
      verbose = verbose,
      engine = engine
    )
  } else {
    image_list <- image
  }

  # open ondisk store
  switch(
    format,
    HDF5ImgArray = {
      if (!file.exists(ondisk_path)) {
        rhdf5::h5createFile(ondisk_path)
      }
      # TODO: is there a better way to check existing groups
      if(!name %in% c("", "/"))
        rhdf5::h5createGroup(ondisk_path, group = name)
    },
    ZarrImgArray = {
      dir.zarr <- gsub(paste0(basename(ondisk_path), "$"), "", ondisk_path)
      open_zarr(dir = dir.zarr, name = basename(ondisk_path))
      zarrcreateGroup(ondisk_path, name)
    }
  )

  # write all levels
  ax <- axes(image_list)
  for (i in seq_len(length(image_list@levels))) {
    img <- image_list[[i]]
    
    # write array
    switch(
      format,
      HDF5ImgArray = {
        image_list[[i]] <-
          HDF5Array::writeHDF5Array(
            img,
            filepath = ondisk_path,
            name = paste0(name, "/", i),
            chunkdim = chunkdim,
            level = level,
            as.sparse = FALSE,
            with.dimnames = FALSE,
            verbose = verbose
          )
      },
      ZarrImgArray = {
        chunk_dim <- stats::setNames(dim(img),ax)
        chunk_dim["x"] <- min(chunk_dim["x"], 2000)
        chunk_dim["y"] <- min(chunk_dim["y"], 2000)
        image_list[[i]] <-
          Rarr::writeZarrArray(
            img,
            zarr_array_path = file.path(ondisk_path, paste0(name, "/", i)),
            chunk_dim = chunk_dim
          )
      },
      InMemoryImgArray = {
        image_list[[i]] <- img
      }
    )
  }

  # return
  return(image_list)
}

####
# Auxiliary ####
####

#' @noRd
.img_create_msg <- function(dim_img, i){
  cat(paste0(
    "Creating level ",
    i,
    " ",
    paste0("(", paste(dim_img, collapse = ","), ")"),
    "\n"
  ))
}
