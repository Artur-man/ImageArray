#' rotate.ImgArray
#'
#' rotate ImgArray image
#'
#' @param object an ImgArray object
#' @param degrees value between 0 and 360 for how many degrees to rotate
#' @export
setMethod("rotate", 
          signature = "ImgArray",
          function(object, degrees){
            
            # validate rotation
            if(!degrees %in% c(0,90,180,270,360)){
              stop("Only rotations of 0,90,180,270,360 degrees are supported!")
            }
            
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
          })

#' aperm
#'
#' aperm ImgArray image
#' 
#' @param a an ImgArray object
#' @param perm perm
#' @export
setMethod("aperm", 
          signature = "ImgArray",
          function(a, perm){
            n.series <- length(a)
            for(i in 1:n.series){
              a[[i]] <- aperm(a[[i]], perm = perm)
            }
            a
          })

#' negate ImgArray image
#'
#' @param object an ImgArray object
#' @export
setMethod("negate", 
          signature = "ImgArray",
          function(object){
            
            n.series <- length(object)
            for(i in 1:n.series){
              object[[i]] <- 255 - object[[i]]
            }
            object
          })

#' flip ImgArray image
#'
#' @param object an ImgArray object
#' @export
setMethod("flip", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in 1:n.series){
              img <- object[[i]]
              dim_img <- dim(img)
              object[[i]] <- img[ , , dim_img[3]:1, drop = FALSE]
            }
            object
          })

#' flop ImgArray image
#'
#' @param object an ImgArray object
#' @export
setMethod("flop", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in 1:n.series){
              img <- object[[i]]
              dim_img <- dim(img)
              object[[i]] <- img[ , dim_img[2]:1, , drop = FALSE]
            }
            object
          })

#' crop ImgArray image
#'
#' @param object an ImgArray object
#' @param ind index list
#' @importFrom utils head tail
#' @export
setMethod("crop", 
          signature = "ImgArray",
          function(object, ind){
            
            # check ind
            if(!is.list(ind))
              stop("'ind' should be a list of integers")
            if((length(dim(object[[1]])) - 1) != length(ind))
              stop("'ind' should be a list of integers")
            
            # crop all images
            n.series <- length(object)
            for(i in 1:n.series){
              img <- object[[i]]
              cur_ind <- lapply(ind, function(curind){
                seq(floor(utils::head(curind,1)/(2^(i-1))), ceiling(utils::tail(curind,1)/(2^(i-1))))
              })
              object[[i]] <- img[, cur_ind[[1]], cur_ind[[2]], drop = FALSE]
            }
            
            object
          })