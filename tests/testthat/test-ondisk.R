library(magick)
library(rhdf5)
library(HDF5Array)
library(Rarr)

dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
output_zarr <- file.path(td, "zarrtest")

# build image array
set.seed(1)
img_arr <- array(
  data = sample(1:255, 2000 * 5000 * 3, replace = TRUE),
  dim = c(2000, 5000, 3)
)
img_raster <- as.raster(img_arr, max = 255)

# read as magick object
img <- magick::image_read(img_raster)

test_that("write image array to disk", {
  # create image array
  imgarray <- writeImageArray(
    img,
    output = output_h5ad,
    name = "image",
    format = "HDF5ImageArray",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(imgarray), 4)
  expect_equal(dim(imgarray), c(3, 5000, 2000))

  # create image array
  unlink(output_zarr, recursive = TRUE)
  imgarray <- writeImageArray(
    img,
    output = output_zarr,
    name = "image",
    format = "ZarrImageArray",
    replace = TRUE,
    verbose = FALSE
  )
  expect_equal(length(imgarray), 4)
  expect_equal(dim(imgarray), c(3, 5000, 2000))

  # refresh
  unlink(output_zarr, recursive = TRUE)
  file.remove(output_h5ad)
})
