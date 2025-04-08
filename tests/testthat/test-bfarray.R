library(magick)
skip_if_not_installed("ggplot2")
library(ggplot2)

# image file
img.file <- system.file("extdata", "xy_12bit__plant.ome.tiff", package = "ImageArray")
img.file2 <- system.file("extdata", "single-channel.ome.tiff", package = "ImageArray")

test_that("bfarray object", {
  
  # create array
  bfa <- BFArray(img.file, series = 1, resolution = 2)
  expect_equal(dim(bfa), c(256,256))
  bfa <- BFArray(img.file, series = 1, resolution = 1)
  expect_equal(dim(bfa), c(512,512))
  
  # methods
  bfa2 <- aperm(bfa,c(2,1))
  expect_equal(bfa2[1,2], bfa[2,1])
  
  # get image info
  getImageInfo(bfa)
  
  # construct imagearray
  img <- createImgArray(img.file, n.series = 1, resolution = 1:2)
  img <- createImgArray(img.file2, n.series = 1, resolution = 1)
  expect_error(img <- createImgArray(img.file2, n.series = 1, resolution = 1:2))
})

test_that("bfarray based ImgArray", {
  
  # create array
  img <- createImgArray(img.file, n.series = 1, resolution = 1:2)
  
  # get image info
  expect_equal(getImageInfo(img), data.frame(width = 512, height = 512))
  
  # construct imagearray
  bfa.raster <- as.raster(img)
  plot(bfa.raster)
})