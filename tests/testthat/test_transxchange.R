# This tests will run without OTP setup.
context("Test the TransXChange Functions")


context("Get the example files")
file_path <- file.path(tempdir(),"gtfs_tests")
dir.create(file_path)
#unzip("inst/extdata/transxchange.zip", exdir = file_path)

test_that("test transxchange data is there", {
  expect_true(file.exists(file.path(.libPaths()[1],"UK2GTFS/extdata/transxchange.zip")))

})



test_that("test transxchange2gtfs", {
  transxchange2gtfs(path_in = file.path(.libPaths()[1],"UK2GTFS/extdata/transxchange.zip"),
                    path_out = file_path,
                    ncores = 2)
  expect_true(file.exists(file.path(file_path,"gtfs.zip")))

})
