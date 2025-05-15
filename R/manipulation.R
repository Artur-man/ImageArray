#' @describeIn ImgArray-methods rotate image array to 90, 180, 270 degrees
#' @exportMethod rotate
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

#' @describeIn ImgArray-methods permute image
#' @exportMethod aperm
setMethod("aperm", 
          signature = "ImgArray",
          function(a, perm){
            n.series <- length(a)
            for(i in seq_len(n.series)){
              a[[i]] <- aperm(a[[i]], perm = perm)
            }
            a
          })

#' @describeIn ImgArray-methods negate image
#' @exportMethod negate
setMethod("negate", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in seq_len(n.series)){
              object[[i]] <- 255 - object[[i]]
            }
            object
          })

#' @describeIn ImgArray-methods vertical flipping image
#' @exportMethod flip
setMethod("flip", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in seq_len(n.series)){
              img <- object[[i]]
              dim_img <- dim(img)
              object[[i]] <- img[ , , dim_img[3]:1, drop = FALSE]
            }
            object
          })

#' @describeIn ImgArray-methods horizontal flipping image
#' @exportMethod flop
setMethod("flop", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in seq_len(n.series)){
              img <- object[[i]]
              dim_img <- dim(img)
              object[[i]] <- img[ , dim_img[2]:1, , drop = FALSE]
            }
            object
          })

#' @describeIn ImgArray-methods cropping image
#' @importFrom utils head tail
#' @exportMethod crop
setMethod("crop", 
          signature = "ImgArray",
          function(object, ind){
            
            # check ind
            if(!is.list(ind))
              stop("'ind' should be a list of integers")
            if((length(dim(object[[1]])) - 1) != length(ind))
              stop("'ind' should be a list of integers")
            check_sequential <- all(vapply(ind, is.sequential, logical(1)))
            if(!check_sequential)
              stop("'ind' should be a list of sequantial integer 
                   vectors (hence slice)")
            
            # crop all images
            n.series <- length(object)
            for(i in seq_len(n.series)){
              img <- object[[i]]
              cur_ind <- lapply(ind, function(curind){
                seq(floor(utils::head(curind,1)/(2^(i-1))), 
                    ceiling(utils::tail(curind,1)/(2^(i-1))))
              })
              object[[i]] <- img[, cur_ind[[1]], cur_ind[[2]], drop = FALSE]
            }
            
            object
          })