#' The ImgArray Class
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

#' @exportClass BFArraySeed BFArray
.BFArraySeed <- setClass("BFArraySeed",
                              contains="Array",
                              representation(
                                filepath="character",
                                series="numeric",
                                resolution="numeric",
                                shape="numeric"
                              ))

setClassUnion(
  "Array_OR_ArraySeed",
  c("Array", "BFArraySeed")
)

#' The BFArray Class
#'
#' @slot series a list of DelayedArray
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
#' @name BFMatrix
#' @exportClass BFMatrix
setClass("BFMatrix", 
         contains=c("BFArray", "DelayedMatrix"))