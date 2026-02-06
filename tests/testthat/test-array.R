library(EBImage)

# image file
img.file <- system.file("images", "sample.png", package = "EBImage")

test_that("check indexing", {
  
  # create ImageArray
  imgarray <- createImageArray(img.file, n.levels = 2)

  # crop
  imgarray_vis <- crop(imgarray, ind = list(100:200, 100:200))
  imgarray_vis <- as.raster(imgarray_vis)
  plot(imgarray_vis)

  # [ method works
  imgarray_vis <- imgarray[100:200,]
  expect_equal(dim(imgarray_vis), c(101, dim(imgarray)[2]))
  imgarray_vis <- imgarray[,100:200]
  expect_equal(dim(imgarray_vis), c(dim(imgarray)[1], 101))
  imgarray_vis <- imgarray[,]
  expect_equal(dim(imgarray_vis), dim(imgarray))
  
  # [ indexing error
  expect_error(imgarray[-100:200,])
  expect_error(imgarray[,-100])
  expect_error(imgarray[1000:3000,])
})