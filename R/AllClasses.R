# #' The ImgArray Class
# #' 
# #' A class for uni-layer and multi-layer DelayedArray-based images
# #' 
# #' @slot series a list of DelayedArray
# #' 
# #' @name ImgArray-class
# #' @rdname ImgArray-class
# #' @exportClass ImgArray
# 
.ImgArray <- setClass(
  Class = "ImgArray",
  slots = c(
    series = "list"
  )
)

# #' @describeIn BFArray-class BFArraySeed class
# #' @exportClass BFArraySeed BFArray
.BFArraySeed <- setClass(
  "BFArraySeed",
  contains = "Array",
  slots = c(
    filepath = "character",
    series = "numeric",
    resolution = "numeric",
    shape = "numeric",
    type = "character"
  )
)

# #' The BFArray Class
# #' 
# #' A class for image arrays read by RBioFormats
# #' 
# #' @slot seed DelayedArray seed
# #' @slot filepath character, path to the image file
# #' @slot series numeric, series ID of the pyramidal image
# #' @slot resolution numeric, resolution ID of the pyramidal image
# #' @slot shape numeric, shape of the image array
# #' @slot type character, data type of the image array
# #' 
# #' @name BFArray-class
# #' @rdname BFArray-class
# #' 
# #' @usage BFArray(arg1, arg2 = default, ...)
# #' @exportClass BFArray
.BFArray <- setClass(
  Class = "BFArray",
  contains = c("DelayedArray"),
  slots = c(seed = "BFArraySeed")
)
