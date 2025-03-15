library(magick)
library(HDF5Array)
library(ZarrArray)

dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
output_zarr <- file.path(td, "zarrtest")

# build image array 
set.seed(1)
mat <- array(data=sample(1:13, 20*50*3, replace = TRUE), dim=c(3, 20, 50))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("path", {
  
  # h5
  mat_list <- writeImgArray(mat_image, 
                              output = output_h5ad, 
                              name = "image",
                              format = "HDF5ImgArray", 
                              replace = TRUE, verbose = FALSE)
  expect_true(file.exists(path(mat_list)))
  
  # change path
  output_h5ad_replace <- gsub("h5test.h5", "h5test2.h5", path(mat_list))
  file.rename(path(mat_list), output_h5ad_replace)
  path(mat_list) <- output_h5ad_replace
  expect_true(file.exists(path(mat_list)))
  
  # zarr
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImgArray(mat_image, 
                              output = output_zarr, 
                              name = "image",
                              format = "ZarrImgArray", 
                              replace = TRUE, verbose = FALSE)
  expect_true(dir.exists(path(mat_list)))
  
  # change path, TODO: zarr5dim not found
  # output_zarr_replace <- gsub("zarrtest.zarr", "zarrtest2.zarr", path(mat_list))
  # file.rename(path(mat_list), output_zarr_replace)
  # path(mat_list) <- output_zarr_replace
  # expect_true(file.exists(path(mat_list)))
})