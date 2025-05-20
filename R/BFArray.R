#' BFArray constructor method
#'
#' A function for creating objects of BFArray class
#' 
#' @param x A BFArray object
#' @param image.file the path to the image read by 
#' RBioFormats
#' @param series the series IDs of the pyramidal image, 
#' typical an integer starting from 1
#' @param resolution the resolution IDs of the 
#' pyramidal image, typical an integer starting from 1
#'
#' @name BFArray-methods
#' @rdname BFArray-methods
#' @importFrom RBioFormats read.metadata
#' 
#' @export
#' @return A BFArray object
BFArray <- function(image.file, series, resolution)
{
  # get metadata
  meta <- RBioFormats::read.metadata(file = image.file, 
                                     filter.metadata = TRUE, 
                                     proprietary.metadata = TRUE)
  len_meta <- vapply(meta@.Data, length, integer(1))
  meta@.Data <- meta@.Data[which(len_meta > 0)]

  # get shape
  series_res_meta <- vapply(meta@.Data, function(x){
    if(!is.null(cm <- x$coreMetadata)) x <- cm
    c(x$series, x$resolutionLevel)
  }, integer(2))
  series_index <- 
    which(series_res_meta[1,] == series & 
            series_res_meta[2,] == resolution)
  if(length(series_index) > 0){
    shape <- vapply(c("sizeC", "sizeX", "sizeY"), function(x){
      md <- meta@.Data[[series_index]]
      if(!is.null(cm <- md$coreMetadata)) md <- cm
      md[[x]]
    }, integer(1), USE.NAMES = FALSE)
    
    seed <- BFArraySeed(filepath = image.file, 
                        series = series, 
                        resolution = resolution, 
                        shape = shape, 
                        type = "double")
    return(.BFArray(seed = DelayedArray(seed)))
  } else {
    stop("Specified resolution was not found in the image!")
  }
}

#' @importFrom S4Vectors new2
BFArraySeed <- function(filepath, series, resolution, shape, type)
{
  S4Vectors::new2("BFArraySeed", 
                  filepath=filepath,
                  series=series, 
                  resolution=resolution, 
                  shape = shape, 
                  type = type)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dim() getter
###

#' @describeIn BFArray-methods dim function for BFArray objects
setMethod("dim", "BFArraySeed", function(x) x@shape)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### type() getter
###

#' @describeIn BFArray-methods type function for BFArray objects
setMethod("type", "BFArraySeed", function(x) x@type)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_array()
###

#' @importFrom RBioFormats read.metadata read.image
#' @importFrom EBImage imageData
.extract_array_from_BFArraySeed <- function(x, index)
{
  temp(x, index)
}

temp <- function(x, index){
  # get metadata
  meta <- RBioFormats::read.metadata(file = x@filepath, 
                                     filter.metadata = TRUE, 
                                     proprietary.metadata = TRUE)
  
  # check for index length
  if(length(index) > 3)
    stop("You cannot get BFArray slices more than 2 dimensions!")
  
  # create slices
  ind <- mapply(function(x,y){
    if(is.null(x)){
      return(seq_len(y))
    } else if(length(x) == 0){
      return(integer(0))
    } else if(length(x) > 0){
      return(x)
    }
  }, index, x@shape, SIMPLIFY = FALSE)
  
  # get slices
  len_ind <- vapply(ind, length, length(ind))
  if(any(len_ind==0)){
    res <- array(dim = len_ind)
    type(res) <- x@type
  } else{
    res <- RBioFormats::read.image(
      file = x@filepath, 
      series = x@series, 
      resolution = x@resolution, 
      subset = list(C = ind[[1]],
                    X = ind[[2]], 
                    Y = ind[[3]]))
    res <- EBImage::imageData(res)
    if(length(dim(res)) != 3){
      res <- array(res, dim = c(1, dim(res)))
    } else{
      res <- aperm(res, perm = c(3,1,2))
    }
  }
  
  return(res)
}

setMethod("extract_array", "BFArraySeed", 
          .extract_array_from_BFArraySeed)

### - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "BFArraySeed",
          function(seed) new_DelayedArray(seed, Class="BFArray"))

### --------------------------------
### BFMatrix
### --------------------------------

## for internal use only.
setMethod("matrixClass", "BFArray", function(x) "BFMatrix")

#' @importFrom methods as new
#' @export
setAs("BFArray", 
      "BFMatrix", 
      function(from) methods::new("BFMatrix", from))
setAs("BFMatrix", 
      "BFArray", 
      function(from) from)
setAs(
  "ANY", "BFMatrix",
  function(from) methods::as(methods::as(from, "BFArray"), "BFMatrix"))