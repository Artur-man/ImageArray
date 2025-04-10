% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{realize,ImgArray-method}
\alias{realize,ImgArray-method}
\title{as.array}
\usage{
\S4method{realize}{ImgArray}(x, max.pixel.size = NULL, min.pixel.size = NULL)
}
\arguments{
\item{x}{an ImgArray object}

\item{max.pixel.size}{maximum pixel size}

\item{min.pixel.size}{minimum pixel size}
}
\value{
An array object
}
\description{
as.array method for ImgArray object
}
\examples{
# get image
img.file <- system.file("extdata", "bird.png", package = "ImageArray")

# create ImgArray
dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
imgarray <- writeImgArray(img.file, 
                          output = output_h5ad, 
                          name = "image",
                          format = "HDF5ImgArray", 
                          replace = TRUE, verbose = FALSE)
imgarray <- realize(imgarray)
}
