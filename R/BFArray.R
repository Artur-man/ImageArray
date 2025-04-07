BFArray <- function(data, series, resolution)
{
  # get metadata
  meta <- RBioFormats::read.metadata(file = data, filter.metadata = TRUE, proprietary.metadata = TRUE)
  len_meta <- vapply(meta@.Data, length, integer(1))
  meta@.Data <- meta@.Data[which(len_meta > 0)]

  # get shape
  series_res_meta <- vapply(meta@.Data, function(x){
    if(!is.null(cm <- x$coreMetadata)) x <- cm
    c(x$series, x$resolutionLevel)
  }, integer(2))
  series_index <- which(series_res_meta[1,] == series & series_res_meta[2,] == resolution)
  if(length(series_index) > 0){
    shape <- vapply(c("sizeX", "sizeY"), function(x){
      md <- meta@.Data[[series_index]]
      if(!is.null(cm <- md$coreMetadata)) md <- cm
      md[[x]]
    }, integer(1), USE.NAMES = FALSE)
    
    seed <- BFArraySeed(filepath = data, series = series, resolution = resolution, shape = shape)
    return(.BFArray(seed = DelayedArray(seed)))
  } else {
    stop("Specified resolution was not found in the image!")
  }
}

#' @importFrom S4Vectors new2
BFArraySeed <- function(filepath, series, resolution, shape)
{
  S4Vectors::new2("BFArraySeed", 
                  filepath=filepath,
                  series=series, 
                  resolution=resolution, 
                  shape = shape)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dim() getter
###

setMethod("dim", "BFArraySeed", function(x) x@shape)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_array()
###

.extract_array_from_BFArraySeed <- function(x, index)
{
  # get metadata
  meta <- RBioFormats::read.metadata(file = x@filepath, filter.metadata = TRUE, proprietary.metadata = TRUE)

  # check for index length
  if(length(index) > 2)
    stop("You cannot get BFArray slices more than 2 dimensions!")
    
  # create slices
  ind <- mapply(function(x,y){
    if(is.null(x)){
      return(1:y)
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
  } else{
    res <- RBioFormats::read.image(file = x@filepath, 
                                   series = x@series, 
                                   resolution = x@resolution, 
                                   subset = list(X = ind[[1]], Y = ind[[2]]))
    res <- EBImage::imageData(res)
  }
  
  return(res)
}

setMethod("extract_array", "BFArraySeed", .extract_array_from_BFArraySeed)

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

#' @export
setAs("BFArray", "BFMatrix", function(from) new("BFMatrix", from))
setAs("BFMatrix", "BFArray", function(from) from)
setAs(
  "ANY", "BFMatrix",
  function(from) as(as(from, "BFArray"), "BFMatrix"))