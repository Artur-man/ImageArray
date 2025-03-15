library(magick)
library(HDF5Array)
library(ZarrArray)

dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
output_zarr <- file.path(td, "zarrtest")

# build image array 
set.seed(1)
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("write image array to disk", {
  
  # create image array
  mat_list <- writeImgArray(mat_image, 
                              output = output_h5ad, 
                              name = "image",
                              format = "HDF5ImgArray", 
                              replace = TRUE, verbose = FALSE)

  # create image array
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImgArray(mat_image, 
                              output = output_zarr, 
                              name = "image",
                              format = "ZarrImgArray", 
                              replace = TRUE, verbose = FALSE)

  # return
  expect_equal(1,1L)
})