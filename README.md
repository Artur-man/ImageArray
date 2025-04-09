# ImageArray

Package for Pyramidal and non-pyramidal images in DelayedArray format 

## Installation

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("BIMSBbioinfo/ImageArray")
```

## Usage

The main usage of the ImageArray is to deliver DelayedArray operations for 
images or a list of images in Pyramidal format. Here, we first store the image
in HDF5. 

``` r
# make random magick image
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)
mat_image <- magick::image_read(mat_raster)

# create image array
dir.create(td <- tempfile())
output_h5ad <- file.path(td, "h5test")
mat_list <- writeImgArray(mat_image, 
                          format = "HDF5ImgArray", 
                          output = output_h5ad, 
                          replace = TRUE)
mat_list
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
mat_list_rotated <- rotate(mat_list, degrees = 90)
mat_list_rotated
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
# crop(mat_list, ind = list(2001:3000, 501:1000))
mat_list_cropped <- mat_list[2001:3000, 501:1000]
mat_list_cropped
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
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)
mat_image <- magick::image_read(mat_raster)

# create image array
dir.create(td <- tempfile())
output_zarr <- file.path(td, "zarrtest")
mat_list <- writeImgArray(mat_image, 
                          format = "ZarrImgArray", 
                          output = output_zarr, 
                          replace = TRUE)
mat_list
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

