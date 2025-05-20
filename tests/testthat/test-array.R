library(magick)
skip_if_not_installed("ggplot2")
library(ggplot2)

# image file
img.file <- system.file("extdata", "bird.png", package = "ImageArray")

test_that("file input, with EBImage", {
  
  # create ImgArray
  imgarray <- createImgArray(img.file, n.series = 2)
  imgarray2 <- as.raster(imgarray, max.pixel.size = 300)
  plot(imgarray2)
  
  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
  # rotate 
  lapply(c(0,90,180,270,360), function(x){
    imgarray_vis <- rotate(imgarray, degree = x)
    imgarray_vis <- as.raster(imgarray_vis)
    plot(imgarray_vis)
  })
  expect_error(imgarray_vis <- rotate(imgarray, degree = 225))
  
  # flip/flop
  imgarray_vis <- flip(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  imgarray_vis <- flop(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
})

test_that("file input, with magick", {
  
  # create ImgArray
  imgarray <- createImgArray(img.file, n.series = 2, engine = "magick-image")
  imgarray2 <- as.raster(imgarray, max.pixel.size = 300)
  plot(imgarray2)
  
  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
  # rotate 
  lapply(c(0,90,180,270,360), function(x){
    imgarray_vis <- rotate(imgarray, degree = x)
    imgarray_vis <- as.raster(imgarray_vis)
    plot(imgarray_vis)
  })
  expect_error(imgarray_vis <- rotate(imgarray, degree = 225))
  
  # flip/flop
  imgarray_vis <- flip(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  imgarray_vis <- flop(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
})

test_that("magick input", {
  
  # create image
  img <- magick::image_read(img.file)
  
  # create ImgArray
  imgarray <- createImgArray(img, n.series = 2)
  imgarray2 <- as.raster(imgarray, max.pixel.size = 300)
  plot(imgarray2)
  
  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
  # rotate 
  lapply(c(0,90,180,270,360), function(x){
    imgarray_vis <- rotate(imgarray, degree = x)
    imgarray_vis <- as.raster(imgarray_vis)
    plot(imgarray_vis)
  })
  expect_error(imgarray_vis <- rotate(imgarray, degree = 225))
  
  # flip/flop
  imgarray_vis <- flip(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  imgarray_vis <- flop(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
})

test_that("bitmap input", {
  
  # create image
  img <- magick::image_read(img.file)
  img <- magick::image_data(img)
  
  # create ImgArray
  imgarray <- createImgArray(img, n.series = 2)
  imgarray2 <- as.raster(imgarray, max.pixel.size = 300)
  plot(imgarray2)
  
  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  
  # rotate 
  lapply(c(0,90,180,270,360), function(x){
    imgarray_vis <- rotate(imgarray, degree = x)
    imgarray_vis <- as.raster(imgarray_vis)
    plot(imgarray_vis)
  })
  expect_error(imgarray_vis <- rotate(imgarray, degree = 225))
  
  # flip/flop
  imgarray_vis <- flip(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
  imgarray_vis <- flop(imgarray)
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)
})