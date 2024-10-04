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