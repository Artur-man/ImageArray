% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImgArray.R
\name{ImgArray-methods}
\alias{ImgArray-methods}
\alias{[[,ImgArray,numeric,ANY-method}
\alias{[[<-,ImgArray,numeric,ANY,ANY-method}
\alias{dim,ImgArray-method}
\alias{length,ImgArray-method}
\title{Methods for ImgArray}
\usage{
\S4method{[[}{ImgArray,numeric,ANY}(x, i)

\S4method{[[}{ImgArray,numeric,ANY,ANY}(x, i, j, ...) <- value

\S4method{dim}{ImgArray}(x)

\S4method{length}{ImgArray}(x)
}
\arguments{
\item{x}{An ImgArray object}

\item{i, value}{Depends on the usage
\describe{
 \item{\code{[[}, \code{[[<-}}{
   Here \code{i} is the level of the image pyramid. You can use the \code{length} function to get the number of the layers in the pyramid
 }
}}

\item{...}{Arguments passed to other methods}
}
\description{
Methods for \code{ImgArray} objects
}
\section{Functions}{
\itemize{
\item \code{x[[i}: Layer access for \code{ImgArray} objects

\item \code{`[[`(x = ImgArray, i = numeric, j = ANY) <- value}: Layer access for \code{ImgArray} objects

\item \code{dim(ImgArray)}: dimensions of an ImgArray

\item \code{length(ImgArray)}: length of an ImgArray

}}
\examples{
# get image
img.file <- system.file("extdata", "bird.png", package = "ImageArray")

# create ImgArray
imgarray <- createImgArray(img.file, n.series = 3)

# access layers
imgarray[[1]]
imgarray[[2]]

# dimensions and length
dim(imgarray)
length(imgarray)
}
\concept{ImgArray}
