library(Rarr)

dir.create(td <- tempfile())
zarr_name <- "test.zarr"
output_zarr <- file.path(td, zarr_name)

test_that("open/create zarr group", {
  
  # open zarr
  open_zarr(dir = td, name = zarr_name)
  expect_true(dir.exists(output_zarr))
  expect_true(file.exists(file.path(output_zarr, ".zgroup")))
  
  # create group one group
  zarrcreateGroup(store = output_zarr, name = "sample")
  expect_true(dir.exists(file.path(output_zarr, "sample")))
  expect_true(file.exists(file.path(output_zarr, "sample", ".zgroup")))
  
  # create nested two groups
  zarrcreateGroup(store = output_zarr, name = "sample1/layer1")
  expect_true(dir.exists(file.path(output_zarr, "sample1")))
  expect_true(file.exists(file.path(output_zarr, "sample1", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "sample1/layer1")))
  expect_true(file.exists(file.path(output_zarr, "sample1/layer1", ".zgroup")))
  
  # create nested three groups
  zarrcreateGroup(store = output_zarr, name = "sample2/layer1/assay1")
  expect_true(dir.exists(file.path(output_zarr, "sample2")))
  expect_true(file.exists(file.path(output_zarr, "sample2", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "sample2/layer1")))
  expect_true(file.exists(file.path(output_zarr, "sample2/layer1", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "sample2/layer1/assay1")))
  expect_true(file.exists(file.path(output_zarr, "sample2/layer1/assay1", ".zgroup")))
  
  # refresh
  unlink(output_zarr, recursive = TRUE)
})
