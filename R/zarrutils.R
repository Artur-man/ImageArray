#' zarrcreateGroup
#'
#' get information of an ImgArray object
#'
#' @param store the location of (zarr) store
#' @param dir directory/location of zarr store
#' @param name name of the zarr store or group
#' 
#' @name zarr-utils
#' @rdname zarr-utils
#' 
#' @concept zarr-utils
#' 
#' @examples
#' # zarr store path
#' dir.create(td <- tempfile())
#' zarr_name <- "test.zarr"
#' output_zarr <- file.path(td, zarr_name)
#' 
#' # open zarr store
#' open_zarr(dir = td, name = zarr_name)
#' 
#' # create group
#' zarrcreateGroup(store = output_zarr, name = "sample")
NULL

#' @describeIn zarr-utils create zarr group
#' @export
#' @return does not return anything, creates a zarr group in the store instead
zarrcreateGroup <- function(store, name){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(rev(seq_len(length(split.name)))[seq_len(2)], 
                         function(x) paste(split.name[seq_len(x)], 
                                           collapse = "/"), 
                         FUN.VALUE = character(1)) 
    if(!dir.exists(file.path(store,split.name[2])))
      zarrcreateGroup(store = store, name = split.name[2])
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  write("{\"zarr_format\":2}", 
        file = file.path(store, 
                         split.name[1], 
                         ".zgroup"))
}

#' @describeIn zarr-utils open zarr stores
#' @export
#' @return does not return anything, opens a zarr store instead
open_zarr <- function(dir, name){
  zarrcreateGroup(store = dir, name = name)
}