library(magick)
skip_if_not_installed("ggplot2")
library(ggplot2)

# image file
img.file <- system.file("extdata", "bird.png", package = "ImageArray")

test_that("read image", {
  
  # with magick-image
  img <- read_image(img.file, engine = "magick-image")
  expect_true(!is.null(img))
  expect_true(inherits(img, "magick-image"))
  
  # with EBImage
  img <- read_image(img.file, engine = "EBImage")
  expect_true(!is.null(img))
  expect_true(inherits(img, "Image"))
  
})