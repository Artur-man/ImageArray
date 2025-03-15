# ImageArray

Package for Pyramidal and non-pyramidal images in DelayedArray format 

## Installation

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("BIMSBbioinfo/ImageArray")
```

## Usage

The main usage of the ImageArray is to deliver DelayedArray operations for images or a list of images in Pyramidal format 

``` r
# make random magick image
mat <- array(data=sample(1:255, 2000*5000*3, replace = TRUE), dim=c(3, 2000, 5000))
mat_raster <- as.raster(aperm(mat, perm = c(2,3,1)), max = 255)
mat_image <- magick::image_read(mat_raster)

# create image array
mat_list <- writeImageArray(mat_image, 
                            format = "HDF5ImageArray", 
                            output = "data/my_image", 
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

```
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

You can even crop images

```
> mat_list_cropped <- crop(mat_list, ind = list(2001:3000, 501:1000))
> mat_list_cropped
```

```
ImgArray Object 
Series 1 of size (3,1000,500) 
Series 2 of size (3,501,251) 
Series 3 of size (3,251,126) 
Series 4 of size (3,126,64)
```