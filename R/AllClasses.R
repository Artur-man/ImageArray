#' The ImgArray Class
#'
#' A class for uni-layer and multi-layer DelayedArray-based images 
#' 
#' @slot series a list of DelayedArray
#'
#' @name ImgArray-class
#' @rdname ImgArray-class
#' @exportClass ImgArray
.ImgArray <- setClass(
  Class="ImgArray",
  slots=c(
    series="list"
  )
)

#' @describeIn BFArray-class BFArraySeed class
#' @exportClass BFArraySeed BFArray
.BFArraySeed <- setClass("BFArraySeed",
                              contains="Array",
                              representation(
                                filepath="character",
                                series="numeric",
                                resolution="numeric",
                                shape="numeric",
                                type="character"
                              ))

setClassUnion(
  "Array_OR_ArraySeed",
  c("Array", "BFArraySeed")
)

#' The BFArray Class
#'
#' A class for image arrays read by RBioFormats
#' 
#' @slot seed DelayedArray seed
#'
#' @name BFArray-class
#' @rdname BFArray-class
#' @exportClass BFArray
.BFArray <- setClass(
  Class="BFArray",
  contains = c("DelayedArray"),
  slots=c(seed="Array_OR_ArraySeed"))

#' The BFMatrix Class
#' 
#' A Matrix class for two dimensional BFArray objects
#' 
#' @name BFMatrix-class
#' @rdname BFMatrix-class
#' @exportClass BFMatrix
setClass("BFMatrix", 
         contains=c("BFArray", "DelayedMatrix"))