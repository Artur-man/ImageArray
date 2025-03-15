#' crop
#'
#' @rdname crop
#' @export crop
crop <- function(object, ind) {
  UseMethod(generic = 'crop', object = object)
}

#' crop
#'
#' crop ImgArray
#'
#' @param object an ImgArray object
#' @param ... arguments passed to other methods.
#'
#' @rdname crop
#' @export crop
setGeneric("crop", function(object, ...) standardGeneric("crop"))

#' rotate
#'
#' rotate ImgArray
#'
#' @param object an ImgArray object
#' @param ... arguments passed to other methods.
#'
#' @rdname rotate
#' @export rotate
setGeneric("rotate", function(object, ...) standardGeneric("rotate"))

#' negate
#'
#' negate ImgArray
#'
#' @param object an ImgArray object
#' @param ... arguments passed to other methods.
#'
#' @rdname negate
#' @export negate
setGeneric("negate", function(object, ...) standardGeneric("negate"))


#' flip
#'
#' flip ImgArray
#'
#' @param object an ImgArray object
#' @param ... arguments passed to other methods.
#'
#' @rdname flip
#' @export flip
setGeneric("flip", function(object, ...) standardGeneric("flip"))

#' flop
#'
#' flop ImgArray
#'
#' @param object an ImgArray object
#' @param ... arguments passed to other methods.
#'
#' @rdname flop
#' @export flop
setGeneric("flop", function(object, ...) standardGeneric("flop"))

