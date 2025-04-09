library(magick)
library(rhdf5)
library(HDF5Array)
library(Rarr)

dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
output_zarr <- file.path(td, "zarrtest")

# build image array 
set.seed(1)
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("manipulate h5 ImgArray", {
  
  # create image array
  mat_list <- writeImgArray(mat_image, 
                              output = output_h5ad, 
                              name = "image",
                              format = "HDF5ImgArray", 
                              replace = TRUE, verbose = FALSE)
  expect_equal(dim(mat_list), c(3,5000,2000))
  
  # aperm
  mat_list_perm <- aperm(mat_list, perm = c(2,1,3))
  expect_equal(dim(mat_list_perm), c(5000,3,2000))
  
  # crop
  mat_list_cropped <- crop(mat_list, ind = list(2001:3000, 1001:2000))
  expect_equal(dim(mat_list_cropped), c(3,1000,1000))
  
  # negate
  mat_list_negated <- negate(mat_list)
  tmp <- realize(mat_list[[1]]) + realize(mat_list_negated[[1]])
  expect_equal(unique(as.vector(tmp)), 255)
  
  # rotate
  mat_list_rotated <- rotate(mat_list, degrees = 90)
  expect_equal(dim(mat_list_rotated), c(3,2000,5000))
  mat_list_rotated <- rotate(mat_list, degrees = 180)
  expect_equal(dim(mat_list_rotated), c(3,5000,2000))
  mat_list_rotated <- rotate(mat_list, degrees = 270)
  expect_equal(dim(mat_list_rotated), c(3,2000,5000))
  expect_error(mat_list_rotated <- rotate(mat_list, degrees = 20))
  
  # flip flop 
  mat_list_flipflop <- flip(mat_list)
  expect_equal(realize(mat_list_flipflop)[1,,][1,], rev(realize(mat_list)[1,,][1,]))
  mat_list_flipflop <- flop(mat_list)
  expect_equal(realize(mat_list_flipflop)[1,,][,1], rev(realize(mat_list)[1,,][,1]))
})

test_that("manipulate zarr ImgArray", {
  
  # create image array
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImgArray(mat_image, 
                            output = output_zarr, 
                            name = "image",
                            format = "ZarrImgArray", 
                            replace = TRUE, verbose = FALSE)
  
  # aperm
  mat_list_perm <- aperm(mat_list, perm = c(2,1,3))
  expect_equal(dim(mat_list_perm), c(5000,3,2000))
  
  # crop
  mat_list_cropped <- crop(mat_list, ind = list(2001:3000, 1001:2000))
  expect_equal(dim(mat_list_cropped), c(3,1000,1000))
  mat_list_cropped <- mat_list[2001:3000, 1001:2000]
  expect_equal(mat_list_cropped,
               crop(mat_list, ind = list(2001:3000, 1001:2000)))
  expect_error(crop(mat_list, ind = list(2001:3000, c(10,20))))
    
  mat_list_negated <- negate(mat_list)
  tmp <- realize(mat_list[[1]]) + realize(mat_list_negated[[1]])
  expect_equal(unique(as.vector(tmp)), 255)
  
  # rotate
  mat_list_rotated <- rotate(mat_list, degrees = 90)
  expect_equal(dim(mat_list_rotated), c(3,2000,5000))
  mat_list_rotated <- rotate(mat_list, degrees = 180)
  expect_equal(dim(mat_list_rotated), c(3,5000,2000))
  mat_list_rotated <- rotate(mat_list, degrees = 270)
  expect_equal(dim(mat_list_rotated), c(3,2000,5000))
  expect_error(mat_list_rotated <- rotate(mat_list, degrees = 20))
  
  # flip flop 
  mat_list_flipflop <- flip(mat_list)
  expect_equal(realize(mat_list_flipflop)[1,,][1,], rev(realize(mat_list)[1,,][1,]))
  mat_list_flipflop <- flop(mat_list)
  expect_equal(realize(mat_list_flipflop)[1,,][,1], rev(realize(mat_list)[1,,][,1]))
})