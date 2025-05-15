#' ImageArray Methods
#'
#' Manipulating images stored as ImgArray objects 
#'
#' @param object An ImgArray object
#' @param a An ImgArray object
#' @param degrees value between 0 and 360 for how many degrees to rotate 
#' @param perm perm
#' @param ind index list
#'
#' @name ImgArray-manipulation
#' @rdname ImgArray-manipulation
#'
#' @concept ImgArray
#' 
#' @examples
#' # get image
#' img.file <- system.file("extdata", "bird.png", package = "ImageArray")
#' 
#' # create ImgArray
#' imgarray <- createImgArray(img.file, n.series = 3)
#' 
#' # features
#' dim(imgarray)
#' length(imgarray)
#' 
#' # manipulate images
#' imgarray <- crop(imgarray, ind = list(100:200, 100:200))
#' imgarray <- rotate(imgarray, degrees = 90)
#' imgarray <- flip(imgarray)
#' imgarray <- flop(imgarray)
NULL

#' @describeIn ImgArray-manipulation rotate image array to 90, 180, 270 degrees
#' @export
#' @returns An ImgArray object
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

#' @describeIn ImgArray-manipulation permute image
#' @export
#' @returns An ImgArray object
setMethod("aperm", 
          signature = "ImgArray",
          function(a, perm){
            n.series <- length(a)
            for(i in seq_len(n.series)){
              a[[i]] <- aperm(a[[i]], perm = perm)
            }
            a
          })

#' @describeIn ImgArray-manipulation negate image
#' @export
#' @returns An ImgArray object
setMethod("negate", 
          signature = "ImgArray",
          function(object){
            n.series <- length(object)
            for(i in seq_len(n.series)){
              object[[i]] <- 255 - object[[i]]
            }
            object
          })

#' @describeIn ImgArray-manipulation vertical flipping image
#' @export
#' @returns An ImgArray object
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

#' @describeIn ImgArray-manipulation horizontal flipping image
#' @export
#' @returns An ImgArray object
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

#' @describeIn ImgArray-manipulation cropping image
#' @importFrom utils head tail
#' @export
#' @returns An ImgArray object
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