% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BFArray.R
\name{BFArray-methods}
\alias{BFArray-methods}
\alias{BFArray}
\alias{dim,BFArraySeed-method}
\alias{type,BFArraySeed-method}
\title{BFArray constructor method}
\usage{
BFArray(image.file, series, resolution)

\S4method{dim}{BFArraySeed}(x)

\S4method{type}{BFArraySeed}(x)
}
\arguments{
\item{image.file}{the path to the image read by 
RBioFormats}

\item{series}{the series IDs of the pyramidal image, 
typical an integer starting from 1}

\item{resolution}{the resolution IDs of the 
pyramidal image, typical an integer starting from 1}

\item{x}{A BFArray object}
}
\value{
A BFArray object
}
\description{
A function for creating objects of BFArray class
}
\section{Functions}{
\itemize{
\item \code{dim(BFArraySeed)}: dim function for BFArray objects

\item \code{type(BFArraySeed)}: type function for BFArray objects

}}
