#' crop
#'
#' @rdname crop
#' @export crop
crop <- function(object, ind) {
  UseMethod(generic = 'crop', object = object)
}

#' rotate
#'
#' @rdname rotate
#' @export rotate
rotate <- function(object, degrees) {
  UseMethod(generic = 'rotate', object = object)
}

#' negate
#'
#' @rdname negate
#' @export negate
negate <- function(object, ing) {
  UseMethod(generic = 'negate', object = object)
}

#' flip
#'
#' @rdname flip
#' @export flip
flip <- function(object) {
  UseMethod(generic = 'flip', object = object)
}

#' flop
#'
#' @rdname flop
#' @export flop
flop <- function(object, ing) {
  UseMethod(generic = 'flop', object = object)
}
