####
# Objects and Classes ####
####

#' The ImageArray Class
#'
#' @slot series a list of DelayedArray
#'
#' @name ImageArray-class
#' @rdname ImageArray-class
#' @exportClass ImageArray
#'
setClass(
  Class="ImageArray",
  
  # can we replace 'Array' with DelayedArray here ?
  contains = c("DelayedArray"),
  
  # temporarily supporting pointers,
  # for the purpose of development...
  slots=c(
    series="list"
  )
)

####
# Methods ####
####


