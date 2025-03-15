library(magick)
library(HDF5Array)
library(ZarrArray)
skip_if_not_installed("ggplot2")
library(ggplot2)

dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
output_zarr <- file.path(td, "zarrtest")

# build image array 
set.seed(1)
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)

# read as magick object
mat_image <- magick::image_read(mat_raster)

test_that("visualize h5 ImgArray", {
  
  # create image array
  mat_list <- writeImgArray(mat_image, 
                              output = output_h5ad, 
                              name = "image",
                              format = "HDF5ImgArray", 
                              replace = TRUE, verbose = FALSE)

  # create raster array
  img_raster <- as.raster(mat_list, max.pixel.size = 2000)
  expect_equal(dim(img_raster), c(500, 1250))
  
  # visualize
  info <- list(width = dim(img_raster)[2], height = dim(img_raster)[1])
  ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(.data[["x"]], .data[["y"]])) + 
    ggplot2::geom_blank() + 
    ggplot2::theme_void() + 
    ggplot2::coord_fixed(expand = FALSE, 
                         xlim = c(0, info$width), 
                         ylim = c(0, info$height)) + 
    ggplot2::annotation_raster(img_raster, 0, info$width, info$height, 0, interpolate = FALSE)
  
})

test_that("visualize zarr ImgArray", {
  
  # create image array
  unlink(output_zarr, recursive = TRUE)
  mat_list <- writeImgArray(mat_image, 
                            output = output_zarr, 
                            name = "image",
                            format = "ZarrImgArray", 
                            replace = TRUE, verbose = FALSE)
  
  # create raster array
  img_raster <- as.raster(mat_list, max.pixel.size = 2000)
  expect_equal(dim(img_raster), c(500, 1250))
  
  # visualize
  info <- list(width = dim(img_raster)[2], height = dim(img_raster)[1])
  ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(.data[["x"]], .data[["y"]])) + 
    ggplot2::geom_blank() + 
    ggplot2::theme_void() + 
    ggplot2::coord_fixed(expand = FALSE, 
                         xlim = c(0, info$width), 
                         ylim = c(0, info$height)) + 
    ggplot2::annotation_raster(img_raster, 0, info$width, info$height, 0, interpolate = FALSE)
})