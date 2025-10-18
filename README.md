# ImageArray

Package for Pyramidal and non-pyramidal images in 
`r Biocpkg("DelayedArray")` format 

## Installation

You can install `r Biocpkg("ImageArray")` from Bioconductor with:

``` r 
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
BiocManager::install("ImageArray")
```

## Usage

The main purpose of the `r Biocpkg("ImageArray")` is to deliver 
`r Biocpkg("DelayedArray")` operations for a list of images in Pyramidal format. 
Here, we first store the image in HDF5. 

``` r
# make random magick image
arr <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(2000, 5000, 3))
img_raster <- as.raster(arr, max = 255)
img <- as.Image(img_raster)

# create image array
dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
imgarray <- writeImgArray(img, 
                         format = "HDF5ImgArray", 
                         output = output_h5ad, 
                         replace = TRUE)
imgarray
```

```
ImgArray Object 
Series 1 of size (3,5000,2000) 
Series 2 of size (3,2500,1000) 
Series 3 of size (3,1250,500) 
Series 4 of size (3,625,250)
```

Operations such as rotate, flip, flop or negate will be conducted on all series

``` r
imgarray_rotated <- rotate(imgarray, degrees = 90)
imgarray_rotated
```

```
ImgArray Object 
Series 1 of size (3,2000,5000) 
Series 2 of size (3,1000,2500) 
Series 3 of size (3,500,1250) 
Series 4 of size (3,250,625) 
```

You can even crop images or slice.

``` r
# crop(imgarray, ind = list(2001:3000, 501:1000))
imgarray_cropped <- imgarray[2001:3000, 501:1000]
imgarray_cropped
```

```
ImgArray Object 
Series 1 of size (3,1000,500) 
Series 2 of size (3,501,251) 
Series 3 of size (3,251,126) 
Series 4 of size (3,126,64)
```

You can also use Zarr for storing the image. 

``` r
# make random magick image
arr <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(2000, 5000, 3))
img_raster <- as.raster(arr, max = 255)
img <- magick::image_read(img_raster)

# create image array
dir.create(td <- tempfile())
output_zarr <- file.path(td, "zarrtest")
imgarray <- writeImgArray(img, 
                          format = "ZarrImgArray", 
                          output = output_zarr, 
                          replace = TRUE)
imgarray
```

```
ImgArray Object 
Series 1 of size (3,5000,2000) 
Series 2 of size (3,2500,1000) 
Series 3 of size (3,1250,500) 
Series 4 of size (3,625,250)
```

Finally, we can parse multiple resolutions of an ome.tiff in an ImgArray object.

``` r
# get ome.tiff
img.file <- system.file("extdata", "xy_12bit__plant.ome.tiff", package = "ImageArray")

# read as ImgArray
img <- createImgArray(img.file, n.series = 1, resolution = 1:2)
```

```
ImgArray Object 
Series 1 of size (512,512) 
Series 2 of size (256,256)
```

