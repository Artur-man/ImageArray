library(magick)
library(EBImage)

# image file
img.file <- system.file("images", "sample.png", package = "EBImage")
img <- magick::image_read(img.file)

test_that("format is given (h5ad)", {
  
  # with extension
  output_h5ad <- tempfile(fileext = ".h5")
  imgarray <- writeImageArray(
    img,
    output = output_h5ad,
    format = "h5",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5ad))
  
  # replace path
  temp_output <- tempdir()
  path(imgarray) <- temp_output
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(temp_output))
  
  # without extension
  output_h5ad <- tempfile(fileext = "")
  imgarray <- writeImageArray(
    img,
    output = output_h5ad,
    format = "h5",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(output_h5ad))
  
  # replace path
  temp_output <- tempdir()
  path(imgarray) <- temp_output
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(temp_output))
  
})

test_that("format is given (zarr)", {

  # with extension
  output_zarr <- tempfile(fileext = ".zarr")
  imgarray <- writeImageArray(
    img,
    output = output_zarr,
    format = "zarr",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)), 
               normalizePath(.collapse_slashes(output_zarr)))
  
  # replace path
  temp_output <- tempfile(fileext = ".zarr")
  path(imgarray) <- temp_output
  expect_equal(
    suppressWarnings(normalizePath(path(imgarray))),
    suppressWarnings(normalizePath(temp_output)))
  
  # without extension
  output_zarr <- tempfile(fileext = "")
  imgarray <- writeImageArray(
    img,
    output = output_zarr,
    format = "zarr",
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_zarr))
})

test_that("format is not given", {
  
  # h5
  output_file <- tempfile(fileext = ".h5")
  imgarray <- writeImageArray(
    img,
    output = output_file,
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
  # zarr
  output_file <- tempfile(fileext = ".zarr")
  imgarray <- writeImageArray(
    img,
    output = output_file,
    replace = TRUE
  )
  expect_equal(normalizePath(path(imgarray)),
               normalizePath(output_file))
  
  # random file throws an error since format cannot be inferred
  output_file <- tempfile()
  expect_error(
    imgarray <- writeImageArray(
      img,
      output = output_file,
      replace = TRUE
    ) 
  )
  
})